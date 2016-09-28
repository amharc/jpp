{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
module Language.FunLog.OrderDecls (orderDecls) where

import Control.Lens hiding (op, indices, levels)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Data.Maybe
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Language.FunLog.Syntax

-- |Maps a 'Name' to 'Name's it uses
type Dependencies = M.HashMap Name (S.HashSet Name)

data Env = Env
    { _envNames :: !(S.HashSet Name)
    , _envCurrent :: !(Maybe Name) -- ^The 'Name' of the symbol being currently declared
    }

-- |Just to disambiguate instances
newtype OdName = OdName Name

data Indices = Indices
    { _idxIndex :: !Int
    , _idxLowlink :: !Int
    }
    deriving Eq

data TarjanState = TarjanState
    { _tsDependencies :: !Dependencies
    , _tsTimer :: !Int
    , _tsIndices :: !(M.HashMap Name Indices)
    , _tsStack :: [Name]
    , _tsStackSet :: !(S.HashSet Name)
    }

-- |Constraint on a 'Monad' the dependency finder is able to work in
type FdMonad m = (MonadReader Env m, MonadState Dependencies m)

-- |Constraint on a 'Monad' the declaration order finder is able to work in
type OdMonad m = (MonadReader Env m, MonadState TarjanState m, MonadWriter [[Name]] m)

makeLenses ''Env
makeLenses ''Indices
makeLenses ''TarjanState

-- |Computes strongly connected components of declarations
--
-- Because polymorphic recursion is prohibited in the pure Hindley-Milner
-- type system, we have to generalise the types of polymorphic bindings before they
-- are used less-polymorphically.
--
-- E.g.:
-- > id = fun x => x. end.
-- > x = id 5.
-- > y = id True.
--
-- Because the type of @id@ should be polymorphic, we do not want to use the nongeneralised
-- type when typechecking @id 5@ and @id True@.
--
-- However, FunLog does not assume anything about the order recursive definitions are made.
-- Therefore, we need to find the graph of mutual (possibly recursive) uses of bindings in
-- another bindings appearing in the same declaration block (i.e. toplevel or a where-clause).
--
-- This graph is then sorted topologically and its strongly connected components are found.
orderDecls :: [Decl] -> [[Decl]]
orderDecls decls = concatMap (namesToDecls M.!) <$> levels
  where
    names = [(name, expr) | DeclName name expr <- decls]
    declNames = [name | DeclSig name _ <- decls]

    levels = execWriter . flip execStateT st . flip runReaderT env $ do
        zoom tsDependencies $ forM_ names $ \(name, expr) ->
            local (envCurrent ?~ name) $ findDeps expr
        forM_ names $ visit . fst
        forM_ declNames visit

    visit name = uses (tsIndices . at name) isJust >>= flip unless (dfs name)

    env = Env (S.fromList $ fst <$> names) Nothing
    st = TarjanState
        { _tsDependencies = []
        , _tsTimer = 0
        , _tsIndices = []
        , _tsStack = []
        , _tsStackSet = []
        }

    namesToDecls :: M.HashMap Name [Decl]
    namesToDecls = flip execState M.empty . forM decls $ \case
        decl@(DeclName name _) -> at name . non [] %= cons decl
        decl@(DeclSig name _) -> at name . non [] %= cons decl
        _ -> pure ()

-- |Performs the Tarjan's strongly connected component algorithm
dfs :: OdMonad m => Name -> m ()
dfs name = do
    idx <- tsTimer <+= 1
    tsIndices . at name ?= Indices idx idx
    tsStack %= cons name
    tsStackSet . contains name .= True

    use (tsDependencies . at name . non S.empty) >>= mapM_ visit

    indices <- use $ indicesOf name
    when (indices ^. idxIndex == indices ^. idxLowlink) $ do
        ((name:) -> scc, _:rest) <- uses tsStack $ break (== name)
        tsStack .= rest
        forM_ scc $ \n -> tsStackSet . contains n .= False
        tell [scc]
  where
    visit name' = use (tsIndices . at name') >>= \case
        Nothing -> do -- name' has not been visited previously
            dfs name'
            lowlink' <- use $ indicesOf name' . idxLowlink
            indicesOf name . idxLowlink %= min lowlink'
        Just indices -> use (tsStackSet . contains name') >>= \case
            True -> indicesOf name . idxLowlink %= min (indices ^. idxIndex)
            False -> pure ()

    indicesOf n = tsIndices . at n . non (Indices (-1) (-1))

class HasDependencies a where
    findDeps :: FdMonad m => a -> m ()

reportDep :: FdMonad m => Name -> m ()
reportDep on = view envCurrent >>= \case
    Nothing -> pure ()
    Just name -> at name . non S.empty . contains on .= True

instance HasDependencies Expr where
    findDeps (ExpLit _) = pure ()
    findDeps (ExpName name) = view (envNames . contains name) >>= flip when (reportDep name)
    findDeps (ExpApp fun arg) = findDeps fun >> findDeps arg
    findDeps (ExpLet pat expr body) = findDeps expr >> with pat (findDeps body)
    findDeps (ExpMatch expr clauses) = findDeps expr >> findDeps clauses
    findDeps (ExpFun clauses) = findDeps clauses
    findDeps (ExpWhere expr decls) = withDecls decls $ findDeps expr
    findDeps (ExpType expr _) = findDeps expr
    findDeps (ExpTuple exprs) = findDeps exprs

instance HasDependencies a => HasDependencies [a] where
    findDeps = mapM_ findDeps

instance HasDependencies Clause where
    findDeps (Clause pats expr) = with pats $ findDeps expr

-- |Removes names used by 'a' from the environment
class With a where
    with :: FdMonad m => a -> m b -> m b

instance With OdName where
    with (OdName name) = local $ envNames . contains name .~ False

instance With Pattern where
    with PatWildcard = id
    with (PatBind name) = with $ OdName name
    with (PatTuple pats) = with pats
    with (PatLit _) = id
    with (PatData _ pats) = with pats
    with (PatType pat _) = with pat

instance With a => With [a] where
    with = flip $ foldr with

withDecls :: FdMonad m => [Decl] -> m a -> m a
withDecls decls op = with names $ findDeps exprs >> op
  where
    names = [OdName name | DeclName name _ <- decls]
    exprs = [expr | DeclName _ expr <- decls]

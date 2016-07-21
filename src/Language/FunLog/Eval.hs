{-@ LIQUID "--notermination" @-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Language.FunLog.Eval where

import Control.Lens hiding (op)
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.HashMap.Strict as M
import Data.IORef
import Data.Maybe
import Language.FunLog.Syntax
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data Value 
    = VInt !Integer
    | VData !Name [LazyValue]
    | VFun !(LazyValue -> IM LazyValue)
    | VApp !LazyValue !LazyValue
    | VIndirect !LazyValue
    | VBlackhole

{-@
measure normality :: Value -> Int
normality (VInt _) = 0
normality (VData _ _) = 0
normality (VFun _) = 0
normality (VApp _ _) = 1
normality (VIndirect _) = 1
normality (VBlackhole) = 1
@-}

{-@
type WHNFValue = { v : Value | ((normality v) == 0) }
@-}

data NFValue = NFInt !Integer | NFData !Name [NFValue] | NFTuple [NFValue] | NFFun

instance Pretty NFValue where
    pPrintPrec _ _ (NFInt i) = integer i
    pPrintPrec _ _ (NFTuple args) = pPrintTuple args
    pPrintPrec _ _ (NFData name []) = text name
    pPrintPrec l r (NFData name args) = maybeParens (r >= 10) $ text name <+> sep (pPrintPrec l 10 <$> args)
    pPrintPrec _ _ NFFun = text "<function>"

instance Show NFValue where
    show = render . pPrint

newtype LazyValue = LazyValue { getLazyValue :: IORef Value }

pattern VTuple vals = VData ":tuple:" vals
pattern VTrue = VData "True" []
pattern VFalse = VData "False" []
pattern VEq = VData "Eq" []
pattern VLt = VData "Lt" []
pattern VGt = VData "Gt" []

data RuntimeError
    = DivByZero
    | MatchFailed
    | Failure
    | NonTermination
    | CompareFunctionValues
    deriving Show

newtype Env = Env
    { _envValues :: M.HashMap Name LazyValue
    }

newtype IM a = IM { getIM :: ExceptT RuntimeError (ReaderT Env IO) a }
    deriving (Functor, Applicative, Monad, MonadReader Env, MonadError RuntimeError, MonadIO)

makeLenses ''Env

lazily :: MonadIO m => Value -> m LazyValue
lazily = liftIO . fmap LazyValue . newIORef

{-@ liftNative2 :: (WHNFValue -> WHNFValue -> IM Value) -> IM LazyValue @-}
liftNative2 :: (Value -> Value -> IM Value) -> IM LazyValue
liftNative2 fun = function $ \arg -> function $ \arg' ->
    lazily <=< join $ fun <$> scrutinise arg <*> scrutinise arg'

runIM :: IM Value -> IO (Either RuntimeError Value)
runIM op = flip runReaderT env . runExceptT . getIM $ do
    plus <- liftNative2 $ \(VInt x) (VInt y) -> pure . VInt $ x + y
    minus <- liftNative2 $ \(VInt x) (VInt y) -> pure . VInt $ x - y
    times <- liftNative2 $ \(VInt x) (VInt y) -> pure . VInt $ x * y
    divide <- liftNative2 $ \(VInt x) -> \case
        VInt 0 -> throwError DivByZero
        VInt y -> pure . VInt $ x `div` y
    comparison <- liftNative2 $ \x y -> cmp x y
    lprinter <- function $ \arg -> printer arg >> lazily (VTuple [])
    sequencer <- function $ \arg -> function $ \arg' -> scrutinise arg >> pure arg'
    failer <- do
        fun <- function . const $ throwError Failure
        arg <- lazily $ VTuple []
        lazily $ VApp fun arg -- fail should be *really* lazy
    let newEnv = M.fromList [("+", plus), ("-", minus), ("*", times), ("/", divide),
            ("compare", comparison), ("print", lprinter), (";", sequencer), ("fail", failer)]

    local (envValues %~ M.union newEnv) op
  where
    env = Env M.empty

    cmp (VInt i) (VInt j) = pure $ case compare i j of
        EQ -> VEq
        LT -> VLt
        GT -> VGt
    cmp (VData name args) (VData name' args') = case compare name name' of
        EQ -> go args args'
        LT -> pure VLt
        GT -> pure VGt
      where
        go [] [] = pure VEq
        go (a:as) (b:bs) = join (cmp <$> scrutinise a <*> scrutinise b) >>= \case
            VEq -> go as bs
            x -> pure x
    cmp (VFun _) _ = throwError CompareFunctionValues

    printer = nf >=> liftIO . print 

function :: (LazyValue -> IM LazyValue) -> IM LazyValue
function fun = do
    env <- ask
    lazily . VFun $ local (const env) . fun

denote :: Expr -> IM LazyValue
denote (ExpLit i) = lazily $ VInt i
denote (ExpName n) = views (envValues . at n) fromJust
denote (ExpApp e e') = (VApp <$> denote e <*> denote e') >>= lazily
denote (ExpLet pat def body) = do
    val <- denote def
    fun <- function $ \arg -> match pat arg (denote body) >>= \case
        Just res -> pure res
        Nothing -> throwError MatchFailed
    lazily $ VApp fun val
denote (ExpMatch scrutinee clauses) = do
    val <- denote scrutinee
    fun <- function $ \arg -> matchClauses clauses [arg]
    lazily $ VApp fun val
denote (ExpFun clauses) = go numArgs []
  where
    numArgs = case head clauses of Clause pats _ -> length pats
    go 0 vals = do
        fun <- function $ scrutinise >=> \(VTuple args) -> matchClauses clauses args
        arg <- lazily . VTuple $ reverse vals
        lazily $ VApp fun arg
    go n vals = function $ \arg ->
        go (n - 1) (arg : vals)
denote (ExpWhere e decls) = denoteDecls decls $ denote e
denote (ExpType e _) = denote e
denote (ExpTuple exprs) = mapM denote exprs >>= lazily . VTuple

denoteDecls :: [Decl] -> IM a -> IM a
denoteDecls decls op = do
    vals <- replicateM (length purged) (lazily VBlackhole)
    local (envValues %~ M.union (M.fromList $ zip names vals)) $ do
        forM_ purged $ \(name, lazyValue) -> do
            val <- lazyValue
            Just (LazyValue ref) <- view $ envValues . at name
            liftIO . writeIORef ref $ VIndirect val
        op
  where
    purged = decls >>= purge
    purge (DeclName name expr) = [(name, denote expr)]
    purge (DeclType _ ctors) = (\(TypeConstr name ty) -> (name, ctorVal name ty [])) <$> ctors
    purge _ = []

    ctorVal :: Name -> Type -> [LazyValue] -> IM LazyValue
    ctorVal name (TyFun _ res) args = function $ \arg -> ctorVal name res (arg : args)
    ctorVal name _ args = lazily . VData name $ reverse args

    names = map fst purged

match :: Pattern -> LazyValue -> IM a -> IM (Maybe a)
match PatWildcard _ op = Just <$> op
match (PatBind name) val op = Just <$> local (envValues . at name ?~ val) op
match (PatTuple pats) val op = scrutinise val >>= \(VTuple vals) -> multimatch pats vals op
match (PatLit i) val op = scrutinise val >>= \case
    VInt vi | i == vi -> Just <$> op
    VInt _ -> pure Nothing
match (PatData name pats) val op = scrutinise val >>= \case
    VData name' vals | name == name' -> multimatch pats vals op 
    VData _ _ -> pure Nothing
match (PatType pat _) val op = match pat val op

multimatch :: MultiPattern -> [LazyValue] -> IM a -> IM (Maybe a)
multimatch [] [] op = Just <$> op
multimatch (pat:pats) (val:vals) op = join <$> match pat val (multimatch pats vals op)

matchClauses :: [Clause] -> [LazyValue] -> IM LazyValue
matchClauses [] _ = throwError MatchFailed
matchClauses (Clause pats ex:cls) vals = multimatch pats vals (denote ex) >>= maybe (matchClauses cls vals) pure

{-@ scrutinise :: LazyValue -> IM WHNFValue @-}
scrutinise :: LazyValue -> IM Value
scrutinise (LazyValue ref) = liftIO (readIORef ref) >>= \case
    VBlackhole -> throwError NonTermination
    VIndirect lv -> blackhole $ scrutinise lv
    VApp fun arg -> blackhole $ scrutinise fun >>= \(VFun fun') -> fun' arg >>= scrutinise
    val -> pure val
  where
    blackhole op = do
        liftIO $ writeIORef ref VBlackhole
        res <- op
        liftIO $ writeIORef ref res
        pure res

nf :: LazyValue -> IM NFValue
nf = scrutinise >=> \case
    VInt i -> pure $ NFInt i
    VTuple args -> NFTuple <$> mapM nf args
    VData name args -> NFData name <$> mapM nf args
    VFun _ -> pure NFFun

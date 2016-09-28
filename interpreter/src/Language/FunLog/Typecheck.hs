{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
module Language.FunLog.Typecheck where

import Control.Lens hiding (op, over)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.List
import qualified Data.HashMap.Strict as M
import Language.FunLog.OrderDecls
import Language.FunLog.Exhaustiveness
import Language.FunLog.Types
import Language.FunLog.Syntax hiding (Type(..))
import qualified Language.FunLog.Syntax as S

readMetaVar :: MetaVar -> TcM (Maybe Type)
readMetaVar (MetaVar _ ref) = liftIO $ readIORef ref

writeMetaVar :: MetaVar -> Type -> TcM ()
writeMetaVar (MetaVar _ ref) ty = liftIO . writeIORef ref $ Just ty

-- |Free type (meta)vars
ftv :: (Eq b, HasFTV b a) => a -> TcM [b]
ftv = fmap nub . ftv'

class HasFTV b a where
    ftv' :: a -> TcM [b]

instance HasFTV Var Var where
    ftv' v = pure [v]

instance HasFTV MetaVar Var where
    ftv' _ = pure []

instance HasFTV Var MetaVar where
    ftv' = readMetaVar >=> maybe (pure []) ftv'

instance HasFTV MetaVar MetaVar where
    ftv' v = readMetaVar v >>= maybe (pure [v]) ftv'

instance (HasFTV a Var, HasFTV a MetaVar) => HasFTV a Type where
    ftv' (TyCtor _ tys) = ftv' tys
    ftv' (TyVar v) = ftv' v
    ftv' (TyMetaVar v) = ftv' v 

instance HasFTV b a => HasFTV b [a] where
    ftv' = fmap concat . mapM ftv'

instance HasFTV Var Scheme where
    ftv' (Forall tvs ty) = (\\) <$> ftv ty <*> ftv tvs

instance HasFTV MetaVar Scheme where
    ftv' (Forall _ ty) = ftv ty

instance HasFTV a Scheme => HasFTV a Env where
    ftv' env = ftv' $ env ^.. envBindings . traversed

type Substitution = M.HashMap Var Type

substitute :: Substitution -> Type -> TcM Type
substitute subst a@(TyVar tv) = maybe (pure a) pure $ tv `M.lookup` subst
substitute subst (TyCtor name args) = TyCtor name <$> mapM (substitute subst) args
substitute subst a@(TyMetaVar mv) = readMetaVar mv >>= maybe (pure a) (substitute subst)

class HasUsedNames a where
    usedNames' :: a -> TcM [Name]

usedNames :: HasUsedNames a => a -> TcM [Name]
usedNames = fmap nub . usedNames'

instance HasUsedNames Type where
    usedNames' (TyCtor _ args) = concat <$> mapM usedNames' args
    usedNames' (TyVar v) = usedNames' v
    usedNames' (TyMetaVar v) = usedNames' v

instance HasUsedNames Scheme where
    usedNames' (Forall tvs ty) = (names ++) <$> usedNames' ty
      where names = [name | BoundTyVar name <- tvs]

instance HasUsedNames Var where
    usedNames' (BoundTyVar name) = pure [name]
    usedNames' (SkolemTyVar _ _) = pure []

instance HasUsedNames MetaVar where
    usedNames' = readMetaVar >=> maybe (pure []) usedNames'

parseType :: S.Type -> TcM Scheme
parseType sty = do
    ty <- go sty
    flip Forall ty <$> ftv ty
  where
    go (S.TyFun arg res) = TyFun <$> go arg <*> go res
    go (S.TyConstr name) = pure $ TyCtor name []
    go (S.TyApp arg res) = go arg >>= \case
        TyCtor name args -> go res >>= \r -> pure $ TyCtor name (args ++ [r])
        _ -> fail "Mailformed type"
    go (S.TyVar name) = pure . TyVar $ BoundTyVar name
    go (S.TyTuple args) = TyTuple <$> mapM go args

parseAndKindCheckType :: S.Type -> TcM Scheme
parseAndKindCheckType sty = do
    scheme@(Forall _ ty) <- parseType sty
    kindCheck ty
    pure scheme

-- |A new metavariable
freshMetaVar :: TcM MetaVar
freshMetaVar = MetaVar <$> (counter <+= 1) <*> liftIO (newIORef Nothing)

-- |Skolemise a bound variable
freshSkolem :: Var -> TcM Var
freshSkolem (BoundTyVar name) = SkolemTyVar name <$> (counter <+= 1)
freshSkolem (SkolemTyVar _ _) = error "Trying to skolemise a skolem variable"

-- |Instantiates a type scheme
instantiate :: Scheme -> TcM Type
instantiate (Forall tvs ty) = do
    fvs <- replicateM (length tvs) $ TyMetaVar <$> freshMetaVar
    substitute (M.fromList $ zip tvs fvs) ty

-- |Generalsies a type, quantifying over all free metavars that are not free
-- in the environment
generaliseIn :: Env -> Type -> TcM Scheme
generaliseIn env ty = do
    over <- (\\) <$> ftv ty <*> ftv env
    used <- usedNames ty
    let new = BoundTyVar <$> take (length over) (allNames \\ used)
    zipWithM_ (\v tv -> writeMetaVar v (TyVar tv)) over new
    Forall new <$> zonk ty

-- |Generalise in the current environment
generalise :: Type -> TcM Scheme
generalise ty = ask >>= flip generaliseIn ty

-- |Removes type substitutions
zonk :: Type -> TcM Type
zonk (TyCtor name args) = TyCtor name <$> mapM zonk args
zonk v@(TyVar _) = pure v
zonk t@(TyMetaVar v) = readMetaVar v >>= \case
    Nothing -> pure t
    Just ty -> do
        res <- zonk ty
        writeMetaVar v res
        pure res

-- |Checks if a type is well-kinded: i.e. all types appearing in it are present in the
-- environment and their arities match. As FunLog currently disallows higher-kinded type
-- vairables, this is sufficient.
kindCheck :: Type -> TcM ()
kindCheck t@(TyFun fun arg) = refineError (WhileKindChecking t) $ kindCheck fun >> kindCheck arg
kindCheck t@(TyTuple args) = refineError (WhileKindChecking t) $ forM_ args kindCheck
kindCheck t@(TyCtor name args) = refineError (WhileKindChecking t) $ do
    view (envTypes . at name) >>= \case
        Nothing -> throwError $ UnboundType name
        Just adt -> unless (length args == adt ^. adtArity) $
            throwError $ WrongArity (adt ^. adtArity) (length args)
    forM_ args kindCheck
kindCheck (TyVar _) = pure ()
kindCheck (TyMetaVar x) = readMetaVar x >>= mapM_ kindCheck

kindCheckEnvironment :: TcM ()
kindCheckEnvironment = join . views envCtors $ mapM_ kindCheckCtor

kindCheckCtor :: Ctor -> TcM ()
kindCheckCtor ctor@Ctor{_ctorType = Forall _ ty} = refineError (WhileKindCheckingCtor ctor) $ kindCheck ty

-- |A infinite supply of variable names
allNames :: [Name]
allNames = [1..] >>= flip replicateM ['a'..'z']

-- |Performs syntactic unification of two types
unify :: Type -> Type -> TcM ()
unify (TyVar tv) (TyVar tv') | tv == tv' = pure ()
unify (TyMetaVar tv) (TyMetaVar tv') | tv == tv' = pure ()
unify (TyMetaVar mv) ty = unifyVar mv ty
unify ty (TyMetaVar mv) = unifyVar mv ty
unify t@(TyCtor name args) t'@(TyCtor name' args')
    | name == name' && length args == length args' =
        flip catchError refine $ zipWithM_ unify args args'
  where
    refine err = WhileUnifying <$> zonk t <*> zonk t' <*> pure err >>= throwError
unify t t' = UnificationError <$> zonk t <*> zonk t' >>= throwError

-- |Unifies a metavar with a type
unifyVar :: MetaVar -> Type -> TcM ()
unifyVar mv ty = readMetaVar mv >>= \case
    Just ty' -> unify ty' ty
    Nothing -> zonk ty >>= \case
        TyMetaVar mv' | mv == mv' -> pure ()
        zonked -> do
            mvs <- ftv zonked
            if mv `elem` mvs
            then throwError $ OccursCheck mv zonked
            else writeMetaVar mv zonked

-- |Checks if its first argument is at least as polymorphic as the second
subsumptionCheck :: Scheme -> Scheme -> TcM ()
subsumptionCheck scheme scheme' = refineError (WhilePerformingSubsumptionCheck scheme scheme') $ do
    (skolems, skolemised) <- skolemise scheme'
    instantiate scheme >>= unify skolemised
    tvs <- (++) <$> ftv scheme <*> ftv scheme'
    let escaped = nub $ filter (`elem` tvs) skolems
    unless (null escaped) . throwError $ SkolemEscape escaped

-- |Converts all bound type vars in a scheme into skolems
skolemise :: Scheme -> TcM ([Var], Type)
skolemise (Forall tvs ty) = do
    skolems <- mapM freshSkolem tvs
    skolemised <- substitute (M.fromList $ zip tvs (TyVar <$> skolems)) ty
    pure (skolems, skolemised)

-- |A bidirectional type-checker
check :: 
    Expr
    -- ^The expression to be type-checked
    -> Type 
    -- ^The type it is required to have (may be a metavar)
    -> TcM ()
check (ExpLit _) ty = unify ty TyInt
check (ExpName n) ty = view (envBindings . at n) >>= \case
    Nothing -> throwError $ UnboundName n
    Just scheme -> instantiate scheme >>= unify ty
check e@(ExpApp fun arg) ty = refineError (WhileCheckingExpr e) $ do
    argty <- TyMetaVar <$> freshMetaVar
    check fun (argty `TyFun` ty)
    check arg argty
check e@(ExpType expr ty') ty = refineError (WhileCheckingExpr e) $ do
    scheme <- parseAndKindCheckType ty'
    ask >>= checkSkolem scheme expr
    instantiate scheme >>= unify ty
check e@(ExpLet pat expr body) ty = refineError (WhileCheckingExpr e) $ do
    (patty, bindings) <- inferPat pat 
    check expr patty
    case M.toList bindings of
        [(name, ty')] -> do
            scheme <- generalise ty'
            local (envBindings . at name ?~ scheme) $ check body ty
        _ -> extend bindings $ check body ty
check e@(ExpMatch expr clauses) ty = refineError (WhileCheckingExpr e) $ do
    argty <- infer expr
    forM_ clauses $ \case
        c@(Clause [pat] body) -> refineError (WhileCheckingClause c) $ do
            bindings <- execStateT (checkPat pat argty) M.empty
            extend bindings $ check body ty
        Clause pats _ -> throwError (WrongArity 1 $ length pats)
    detectNonexhaustive clauses
check e@(ExpFun clauses) ty = refineError (WhileCheckingExpr e) $ do
    case nub [length pats | Clause pats _ <- clauses] of
        [_] -> pure ()
        (arity:arity':_) -> throwError $ WrongArity arity arity'
        [] -> error "Empty clause list"
    forM_ clauses $ \c@(Clause pats body) -> refineError (WhileCheckingClause c) $ do
        resty:argtys <- replicateM (1 + length pats) (TyMetaVar <$> freshMetaVar)
        unify ty (foldr TyFun resty argtys)
        bindings <- execStateT (zipWithM checkPat pats argtys) M.empty
        extend bindings $ check body resty
    detectNonexhaustive clauses
check (ExpWhere expr decls) ty = declare DeclLocal decls $ check expr ty
check e@(ExpTuple args) ty = refineError (WhileCheckingExpr e) $ do
    argtys <- replicateM (length args) (TyMetaVar <$> freshMetaVar)
    unify ty (TyTuple argtys)
    zipWithM_ check args argtys

-- |Checks if an expression can be typed exactly with the (user-provided) scheme.
-- Proper skolemisation of type variables is performed.
checkSkolem :: Scheme -> Expr -> Env -> TcM ()
checkSkolem scheme expr env = do
    (skolems, skolemised) <- skolemise scheme
    check expr skolemised

    tvs <- (++) <$> ftv env <*> ftv scheme
    let escaped = nub $ filter (`elem` tvs) skolems
    unless (null escaped) . throwError $ SkolemEscape escaped

-- |Fails if pattern match is not exhaustive.
detectNonexhaustive :: [Clause] -> TcM ()
detectNonexhaustive clauses = unmatched [pats | Clause pats _ <- clauses] >>= \case
    [] -> pure ()
    skels -> throwError $ Unmatched skels

data DeclMode
    = DeclLocal  -- ^No type declarations allowed (where-block)
    | DeclGlobal -- ^All declarations allowed (top-level)
    deriving Eq

-- |Checks the declarations and extends the environment.
declare :: DeclMode -> [Decl] -> TcM a -> TcM a
declare mode decls op = withTypes decls $ do
    when (mode == DeclGlobal) kindCheckEnvironment
    foldr withLayer op (orderDecls decls)
  where
    withType :: Decl -> TcM a -> TcM a
    withType decl@(DeclType _ _) = case mode of
        DeclLocal -> const $ fail "Illegal local type declaration"
        DeclGlobal -> addTypes decl
    withType _ = id

    withTypes :: [Decl] -> TcM a -> TcM a
    withTypes decl act = foldr withType act decl

-- |Adds a strongly-connected component (recursive group) of declarations at once
withLayer :: [Decl] -> TcM a -> TcM a
withLayer decls op = do
    meta <- execStateT (forM_ decls addMeta >> forM_ decls addSig) M.empty
    env0 <- ask
    extendSchemes meta $ do
        env <- ask
        forM_ exprs $ \(name, expr) -> refineError (WhileCheckingDeclaration (DeclName name expr)) $ do
            Just scheme <- view $ envBindings . at name
            checkSkolem scheme expr env
        tys <- forM meta $ instantiate >=> generaliseIn env0
        extendSchemes tys op
  where
    addMeta :: Decl -> StateT (M.HashMap Name Scheme) TcM ()
    addMeta (DeclType _ _) = pure ()
    addMeta decl@(DeclName name _) = use (at name) >>= \case
        Nothing -> do
            tv <- lift freshMetaVar
            at name ?= Forall [] (TyMetaVar tv)
        Just _ -> throwError . WhileCheckingDeclaration decl $ MultipleDeclarations name
    addMeta (DeclSig _ _) = pure ()

    addSig :: Decl -> StateT (M.HashMap Name Scheme) TcM ()
    addSig d@(DeclSig name sig) = refineError (WhileCheckingDeclaration d) $ use (at name) >>= \case
        Nothing -> throwError $ SignatureWithoutBinding name
        Just (Forall [] (TyMetaVar _)) -> do -- no signature was found before
            scheme <- lift $ parseAndKindCheckType sig
            at name ?= scheme
        Just _ -> throwError $ MultipleSignatures name
    addSig _ = pure ()

    exprs = [(name, expr) | DeclName name expr <- decls]

addTypes :: Decl -> TcM a -> TcM a
addTypes d@(DeclType name ctors) op = do
    refineError (WhileCheckingDeclaration d) $ 
        view (envTypes . at name) >>= maybe (pure ()) (const . throwError $ MultipleDeclarations name)
    ctors' <- M.fromList <$> mapM parseCtor ctors
    views envCtors (M.elems . M.intersection ctors') >>= \case
        [] -> local (envCtors %~ M.union ctors') $ do
            let ctorsList = M.elems ctors'
            arity:arities <- forM ctorsList $ \ctor -> refineError (WhileCheckingConstructor ctor) $
                typeArityFromCtor name ctor 
            case filter (/= arity) arities of
                [] -> local (envTypes . at name ?~ ADT ctorsList arity) $
                    extendSchemes (view ctorType <$> ctors') op
                arity':_ -> throwError . WhileCheckingTypeDeclaration d $ WrongArity arity arity'
        (ctor:_) -> throwError . MultipleDeclarations $ ctor ^. ctorName
addTypes _ op = op

typeArityFromCtor :: Name -> Ctor -> TcM Int
typeArityFromCtor tyname = views ctorType $ \(Forall tvs x) -> go tvs x
  where
    go tvs (TyFun _ res) = go tvs res
    go tvs (TyCtor name args) = do
        when (name /= tyname) . throwError $ ConstructorReturnTypeNameMismatch tyname name
        let vars = [var | TyVar var <- args]
        when (length vars /= length args) $ throwError ConstructorGADT
        when (sort tvs /= sort vars) $ throwError ConstructorGADT
        pure $ length vars
    go _ _ = error "Wrong constructor type in typeArityFromCtor"

parseCtor :: TypeConstr -> TcM (Name, Ctor)
parseCtor (TypeConstr name ty) = do
    scheme <- parseType ty
    pure (name, Ctor name scheme (getArity ty))
  where
    getArity (S.TyFun _ x) = 1 + getArity x
    getArity _ = 0

extend :: M.HashMap Name Type -> TcM a -> TcM a
extend = extendSchemes . fmap (Forall [])

extendSchemes :: M.HashMap Name Scheme -> TcM a -> TcM a
extendSchemes bindings = local $ envBindings %~ M.union bindings

inferPat :: Pattern -> TcM (Type, M.HashMap Name Type)
inferPat pat = do
    mv <- TyMetaVar <$> freshMetaVar
    (mv,) <$> execStateT (checkPat pat mv) M.empty

checkPat :: Pattern -> Type -> StateT (M.HashMap Name Type) TcM ()
checkPat PatWildcard _ = pure ()
checkPat (PatBind name) ty = do
    st <- get
    if name `M.member` st
    then throwError $ MultipleDeclarations name
    else modify (M.insert name ty)
checkPat p@(PatTuple pats) ty = refineError (WhileCheckingPattern p) $
    zipWithM_ checkPat pats <=< lift $ do
        argtys <- replicateM (length pats) (TyMetaVar <$> freshMetaVar)
        unify ty (TyTuple argtys)
        pure argtys
checkPat (PatLit _) ty = lift $ unify ty TyInt
checkPat p@(PatData name args) ty = refineError (WhileCheckingPattern p) $ view (envCtors . at name) >>= \case
    Nothing -> throwError $ UnboundPattern name
    Just (Ctor _ scheme arity) -> do
        unless (arity == length args) $ throwError $ WrongArity arity (length args)
        argtys <- lift $ do
            ctorty <- instantiate scheme
            argtys <- replicateM (length args) (TyMetaVar <$> freshMetaVar)
            unify ctorty (foldr TyFun ty argtys)
            pure argtys
        zipWithM_ checkPat args argtys
checkPat p@(PatType pat ty') ty = refineError (WhileCheckingPattern p) $
    lift (parseAndKindCheckType ty') >>= \case
        Forall [] ty'' -> do
            lift $ unify ty ty''
            checkPat pat ty
        _ -> throwError PolymorphicPattern

infer :: Expr -> TcM Type
infer expr = do
    mv <- TyMetaVar <$> freshMetaVar
    check expr mv
    pure mv

runTcM :: TcM a -> IO (Either TypeCheckerError a)
runTcM = flip evalStateT (Counter 0) . flip runReaderT env . runExceptT
  where
    env = Env 
        (M.fromList $ printers : sequencers : fails : polyOperators : intOperators)
        (M.fromList [("Lt", lt), ("Gt", gt), ("Eq", eq)])
        (M.fromList [("int", ADT [] 0), ("ordering", ADT [lt, gt, eq] 0)])
    intOperators = (, Forall [] (TyInt `TyFun` TyInt `TyFun` TyInt)) <$> ["+", "-", "*", "/"]
    polyOperators = ("compare", Forall [var] $ TyVar var `TyFun` TyVar var `TyFun` TyOrdering)
    printers = ("print", Forall [var] $ TyVar var `TyFun` TyTuple [])
    sequencers = (";", Forall [var, var'] $ TyVar var `TyFun` TyVar var' `TyFun` TyVar var')
    fails = ("fail", Forall [var] $ TyVar var)
    var = BoundTyVar "a"
    var' = BoundTyVar "b"
    lt = Ctor "Lt" (Forall [] TyOrdering) 0
    gt = Ctor "Gt" (Forall [] TyOrdering) 0
    eq = Ctor "Eq" (Forall [] TyOrdering) 0

refineError :: MonadError TypeCheckerError m => (TypeCheckerError -> TypeCheckerError) -> m a -> m a
refineError ref = flip catchError (throwError . ref)

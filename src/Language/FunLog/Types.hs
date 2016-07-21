{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.FunLog.Types where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.IORef
import GHC.Generics
import Language.FunLog.Syntax hiding (Type(..))
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

-- |Represents a symbolic representation of an algebraic data value
data DataSkel
    = DsAny -- ^Anything
    | DsLargeInt -- ^A large integer
    | DsCtor Name [DataSkel] -- ^Constructor
    | DsTuple [DataSkel]
    deriving Eq

data Env = Env
    { _envBindings :: M.HashMap Name Scheme
    , _envCtors :: M.HashMap Name Ctor
    , _envTypes :: M.HashMap Name ADT
    }

newtype Counter = Counter
    { _counter :: Int
    }
    deriving (Eq, Generic, Hashable, Ord)

type TcM = ExceptT TypeCheckerError (ReaderT Env (StateT Counter IO))

data Ctor = Ctor
    { _ctorName :: String
    , _ctorType :: Scheme
    , _ctorArity :: Int
    }

data ADT = ADT
    { _adtCtors :: [Ctor]
    , _adtArity :: Int -- ^A poor's man kind
    }

data Type
    = TyCtor Name [Type]
    | TyVar Var
    | TyMetaVar MetaVar

data TypeCheckerError
    = UnificationError Type Type
    | OccursCheck MetaVar Type
    | UnboundName Name
    | UnboundType Name
    | MultipleDeclarations Name
    | MultipleSignatures Name
    | SignatureWithoutBinding Name
    | UnboundPattern Name
    | SkolemEscape [Var]
    | ConstructorReturnTypeNameMismatch Name Name
    | ConstructorGADT
    | WrongArity Int Int
    | PolymorphicPattern
    | KindMismatch
    | Unmatched [[DataSkel]]
    | WhileCheckingExpr Expr TypeCheckerError
    | WhileCheckingPattern Pattern TypeCheckerError
    | WhileCheckingClause Clause TypeCheckerError
    | WhileUnifying Type Type TypeCheckerError
    | WhilePerformingSubsumptionCheck Scheme Scheme TypeCheckerError
    | WhileCheckingTypeDeclaration Decl TypeCheckerError
    | WhileCheckingDeclaration Decl TypeCheckerError
    | WhileCheckingConstructor Ctor TypeCheckerError
    | WhileKindChecking Type TypeCheckerError
    | WhileKindCheckingCtor Ctor TypeCheckerError

pattern TyFun fun arg = TyCtor "->" [fun, arg]
infixr 9 `TyFun`
pattern TyInt = TyCtor "int" []
pattern TyOrdering = TyCtor "ordering" []
pattern TyTuple args = TyCtor ":tuple:" args

data MetaVar = MetaVar Int (IORef (Maybe Type))
    deriving Eq

data Var
    = BoundTyVar Name
    | SkolemTyVar Name Int
    deriving (Eq, Generic, Hashable, Ord)

data Scheme = Forall [Var] Type

makeLenses ''Env
makeLenses ''Ctor
makeLenses ''ADT
makeLenses ''Counter

instance Pretty Var where
    pPrint (BoundTyVar name) = text name
    pPrint (SkolemTyVar name cnt) = text name <> text "%" <> int cnt

instance Show Var where
    show = render . pPrint

instance Pretty MetaVar where
    pPrint (MetaVar cnt _) = text "%" <> int cnt

instance Show MetaVar where
    show = render . pPrint

arrPrec :: Rational
arrPrec = 10

ctorPrec :: Rational
ctorPrec = 20

instance Pretty Type where
    pPrintPrec l r (TyVar var) = pPrintPrec l r var
    pPrintPrec l r (TyMetaVar var) = pPrintPrec l r var
    pPrintPrec l r (TyFun arg res) = maybeParens (r >= arrPrec) $ pPrintPrec l arrPrec arg <+> text "->" <+> pPrintPrec l (arrPrec - 1) res
    pPrintPrec _ _ (TyTuple args) = pPrintTuple args
    pPrintPrec _ _ (TyCtor name []) = text name
    pPrintPrec l r (TyCtor name args) = maybeParens (r >= ctorPrec) $ text name <+> sep (pPrintPrec l ctorPrec <$> args)

instance Show Type where
    show = render . pPrint

instance Pretty Scheme where
    pPrint (Forall tvs ty) = braces (hsep $ pPrint <$> tvs) <+> pPrint ty

instance Show Scheme where
    show = render . pPrint

instance Pretty Ctor where
    pPrint c = sep [text (c ^. ctorName), colon, pPrint (c ^. ctorType)]

instance Show Ctor where
    show = render . pPrint

instance Pretty TypeCheckerError where
    pPrint (UnificationError t t') = vcat
        [ text "Could not unify"
        , nest 2 $ pPrint t
        , text "with"
        , nest 2 $ pPrint t'
        ]
    pPrint (OccursCheck var t) = vcat
        [ text "Occurs check failed." <+> pPrint var <+> text "occurs inside"
        , nest 2 $ pPrint t
        ]
    pPrint (UnboundName name) = text "Unbound name: " <+> text name
    pPrint (UnboundType name) = text "Unbound type: " <+> text name
    pPrint (MultipleDeclarations name) = text "Multiple declarations of: " <+> text name
    pPrint (MultipleSignatures name) = text "Multiple type signatures for: " <+> text name
    pPrint (SignatureWithoutBinding name) = text "Signature without a corresponding binding for: " <+> text name
    pPrint (UnboundPattern name) = text "Unbound pattern: " <+> text name
    pPrint (WrongArity expected got) = text "Wrong arity, expected:" <+> int expected <> text ", got" <+> int got
    pPrint (SkolemEscape skolems) = vcat
        [ text "Skolem escape check failed. The following variables would escape their scope:"
        , nest 2 . cat $ map pPrint skolems
        ]
    pPrint (ConstructorReturnTypeNameMismatch expected got) = vcat
        [ text "Data constructor returns the wrong type."
        , text "Expected:" <+> text expected
        , text "Got:" <+> text got
        ]
    pPrint ConstructorGADT = text "Illegal generalised algebraic data constructor."
    pPrint KindMismatch = text "Kind mismatch."
    pPrint PolymorphicPattern = text "Polymorphic pattern."
    pPrint (Unmatched skels) = hang (text "Unmatched patterns:") 2 $ vcat (hsep . map (pPrintPrec prettyNormal ctorPrec) <$> skels)
    pPrint (WhileCheckingExpr expr tc) = hang (pPrint tc) 2 $ vcat
        [ text "While inferring type of:"
        , nest 2 $ pPrint expr
        ]
    pPrint (WhileCheckingPattern pat tc) = hang (pPrint tc) 2 $ vcat
        [ text "While inferring type of pattern:"
        , nest 2 $ pPrint pat
        ]
    pPrint (WhileCheckingClause clause tc) = hang (pPrint tc) 2 $ vcat
        [ text "While inferring type of clause:"
        , nest 2 $ pPrint clause
        ]
    pPrint (WhileUnifying t t' tc) = hang (pPrint tc) 2 $ vcat
        [ text "While unifying"
        , nest 2 $ pPrint t
        , text "with"
        , nest 2 $ pPrint t'
        ]
    pPrint (WhilePerformingSubsumptionCheck t t' tc) = hang (pPrint tc) 2 $ vcat
        [ text "While checking that the inferred type:"
        , nest 2 $ pPrint t
        , text "is at least as polymorphic as"
        , nest 2 $ pPrint t'
        ]
    pPrint (WhileCheckingTypeDeclaration decl tc) = hang (pPrint tc) 2 $ vcat
        [ text "While checking type declaration:"
        , nest 2 $ pPrint decl
        ]
    pPrint (WhileCheckingDeclaration decl tc) = hang (pPrint tc) 2 $ vcat
        [ text "While checking declaration:"
        , nest 2 $ pPrint decl
        ]
    pPrint (WhileCheckingConstructor ctor tc) = hang (pPrint tc) 2 $ vcat
        [ text "While checking data constructor:"
        , nest 2 $ pPrint ctor
        ]
    pPrint (WhileKindChecking ty tc) = hang (pPrint tc) 2 $ vcat
        [ text "While checking that the following type is well-kinded:"
        , nest 2 $ pPrint ty
        ]
    pPrint (WhileKindCheckingCtor ctor tc) = hang (pPrint tc) 2 $ vcat
        [ text "While checking that the following constructor is well-kinded:"
        , nest 2 $ pPrint ctor
        ]

instance Show TypeCheckerError where
    show = render . pPrint

deriving instance Show ADT
deriving instance Show Env

instance Pretty DataSkel where
    pPrintPrec _ _ DsAny = text "_"
    pPrintPrec _ _ DsLargeInt = text "[large integer]"
    pPrintPrec _ _ (DsTuple args) = pPrintTuple args
    pPrintPrec _ _ (DsCtor name []) = text name
    pPrintPrec l r (DsCtor name args) = maybeParens (r >= ctorPrec) $ text name <+> sep (pPrintPrec l ctorPrec <$> args)

instance Show DataSkel where
    show = render . pPrint


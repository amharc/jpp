{-# LANGUAGE DeriveDataTypeable #-}
module Language.FunLog.Syntax 
    ( Name
    , Decl(..)
    , Program(..)
    , Pattern(..)
    , MultiPattern
    , Type(..)
    , Expr(..)
    , PredClause(..)
    , Clause(..)
    , TypeConstr(..)
    , operators
    , Associativity(..)
    , pPrintTuple
    ) where

import Data.Data
import Data.List
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

type Name = String

data Decl = DeclName Name Expr
          | DeclType Name [TypeConstr]
          | DeclSig Name Type
          deriving (Eq, Data, Typeable)

newtype Program = Program [Decl]
    deriving (Data, Typeable)

data Pattern = PatWildcard
             | PatBind Name
             | PatTuple [Pattern]
             | PatLit Integer
             | PatData Name [Pattern]
             | PatType Pattern Type
             deriving (Eq, Data, Typeable)

type MultiPattern = [Pattern]

data Type = TyFun Type Type
          | TyApp Type Type
          | TyConstr Name
          | TyVar Name
          | TyTuple [Type]
          deriving (Eq, Data, Typeable)

data Expr = ExpLit Integer
          | ExpName Name
          | ExpApp Expr Expr
          | ExpLet Pattern Expr Expr
          | ExpMatch Expr [Clause]
          | ExpFun [Clause]
          | ExpWhere Expr [Decl]
          | ExpType Expr Type
          | ExpTuple [Expr]
          deriving (Eq, Data, Typeable)

{-@ type ClauseList = { v : [Clause] | ((len v) > 0) } @-}

{-@
data Expr = ExpLit Integer
          | ExpName Name
          | ExpApp { expAppFun :: Expr, expAppArg :: Expr }
          | ExpLet { expLetPattern :: Pattern, expLetExpr :: Expr, expLetBody :: Expr }
          | ExpMatch { expMatchExpr :: Expr, expMatchClauses :: ClauseList }
          | ExpFun ClauseList
          | ExpType { expTypeExpr :: Expr, expTypeType :: Type }
          | ExpTuple { expTupleArgs :: { v : [Expr] | ((len v) /= 1) } }
@-}

data PredClause = PredClause [Name] MultiPattern Expr
    deriving (Eq, Data, Typeable)

data Clause = Clause MultiPattern Expr
    deriving (Eq, Data, Typeable)

data TypeConstr = TypeConstr Name Type
    deriving (Eq, Data, Typeable)

data Associativity = AssociativityLeft | AssociativityNone | AssociativityRight

{-# ANN operators "HLint: ignore Use String" #-}
operators :: [([Char], Associativity)]
operators = [ ("*/%", AssociativityLeft)
            , ("+-", AssociativityLeft)
            , ("\\=<>", AssociativityNone)
            , ("!&|^", AssociativityLeft)
            , (";", AssociativityRight)
            , (",", AssociativityRight)
            , ("$", AssociativityRight)
            ]

dot :: Doc
dot = char '.'

lowPrec :: Rational
lowPrec = 0

opPrec :: Rational
opPrec = 5

appPrec :: Rational
appPrec = 7

arrPrec :: Rational
arrPrec = 10

ctorPrec :: Rational
ctorPrec = 20

eps :: Rational
eps = 1 / (1 + genericLength operators)

instance Pretty Decl where
    pPrint (DeclName name expr) = prettyName name <+> equals <+> pPrint expr <> dot
    pPrint (DeclType name tcs) = text name <+> equals <+> vcat
        [ text "type"
        , nest 2 (vcat $ map pPrint tcs)
        , text "end" <> dot
        ]
    pPrint (DeclSig name ty) = prettyName name <+> colon <+> pPrint ty <> dot

instance Show Decl where
    show = render . pPrint

instance Pretty Program where
    pPrint (Program decls) = vcat $ map pPrint decls

instance Show Program where
    show = render . pPrint

instance Pretty Pattern where
    pPrintPrec _ _ PatWildcard = char '_'
    pPrintPrec _ _ (PatBind name) = prettyName name
    pPrintPrec _ _ (PatTuple elts) = pPrintTuple elts
    pPrintPrec _ _ (PatLit i) = integer i
    pPrintPrec _ _ (PatData name []) = text name
    pPrintPrec l r (PatData name args) = maybeParens (r >= ctorPrec) $ text name <+> sep (pPrintPrec l ctorPrec <$> args)
    pPrintPrec l _ (PatType pat typ) = parens (pPrintPrec l lowPrec pat <+> colon <+> pPrintPrec l lowPrec typ)

instance Show Pattern where
    show = render . pPrint

instance Pretty Type where
    pPrintPrec l r (TyFun arg res) = maybeParens (r >= arrPrec) $ pPrintPrec l arrPrec arg <+> text "->" <+> pPrintPrec l (arrPrec - 1) res
    pPrintPrec l r (TyApp fun arg) = maybeParens (r >= ctorPrec) $ pPrintPrec l (ctorPrec - 1) fun <+> pPrintPrec l ctorPrec arg
    pPrintPrec _ _ (TyConstr name) = text name
    pPrintPrec _ _ (TyVar name) = text name
    pPrintPrec _ _ (TyTuple tys) = pPrintTuple tys

instance Show Type where
    show = render . pPrint

findOperator :: String -> Maybe (Rational, Associativity)
findOperator str = case val of
    [] -> Nothing
    [(i, assoc)] -> Just (opPrec + 1 - i / genericLength operators, assoc)
    _ -> error "Multiple precedencies"
  where
    val = [(i, assoc) | (i, (start, assoc)) <- [0..] `zip` operators, head str `elem` start]

prettyName :: String -> Doc
prettyName name
    | Just _ <- findOperator name = text "operator" <+> text name
    | otherwise = text name

instance Pretty Expr where
    pPrintPrec _ _ (ExpLit i) = integer i
    pPrintPrec l r (ExpApp (ExpApp (ExpName op) lhs) rhs)
        | Just (prec, assoc) <- findOperator op =
            let (precRight, precLeft) = case assoc of
                    AssociativityLeft -> (prec, prec - eps)
                    AssociativityNone -> (prec, prec)
                    AssociativityRight -> (prec - eps, prec)
            in maybeParens (r >= prec) $ sep
                [ pPrintPrec l precLeft lhs
                , text op
                , pPrintPrec l precRight rhs
                ]
    pPrintPrec _ _ (ExpName name) = prettyName name
    pPrintPrec l r (ExpApp fun arg) = maybeParens (r >= appPrec) $
        pPrintPrec l (appPrec - eps) fun <+> pPrintPrec l appPrec arg
    pPrintPrec _ _ (ExpLet pat ex body) = vcat
        [ text "let" <+> pPrint pat <+> equals <+> pPrint ex
        , text "in" <+> pPrint body
        ]
    pPrintPrec _ _ (ExpMatch ex clauses) = vcat
        [ text "match" <+> pPrint ex <+> text "with"
        , nest 2 . vcat $ map pPrint clauses
        , text "end"
        ]
    pPrintPrec _ _ (ExpFun clauses) = vcat
        [ text "fun"
        , nest 2 . vcat $ map pPrint clauses
        , text "end"
        ]
    pPrintPrec _ _ (ExpWhere ex decls) = vcat
        [ pPrint ex
        , nest 2 $ vcat [ text "where"
                        , nest 2 . vcat $ map pPrint decls
                        , text "end"
                        ]
        ]
    pPrintPrec _ _ (ExpType ex ty) = parens $ pPrint ex <+> colon <+> pPrint ty
    pPrintPrec _ _ (ExpTuple elts) = pPrintTuple elts

instance Show Expr where
    show = render . pPrint

instance Pretty TypeConstr where
    pPrint (TypeConstr name ty) = text name <+> colon <+> pPrint ty <> dot

instance Show TypeConstr where
    show = render . pPrint

instance Pretty Clause where
    pPrintPrec l _ (Clause pats ex) = hsep (pPrintPrec l ctorPrec <$> pats) <+> text "=>" <+> pPrint ex <> dot

instance Show Clause where
    show = render . pPrint

pPrintTuple :: Pretty a => [a] -> Doc
pPrintTuple = parens . sep . intersperse (char '#') . map pPrint

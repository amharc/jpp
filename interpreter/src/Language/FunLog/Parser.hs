{-@ LIQUID "--notermination" @-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Language.FunLog.Parser where

import Control.Monad
import Data.Functor.Identity
import Language.FunLog.Syntax
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Parsec.Language (haskellStyle)
import qualified Text.Parsec.Token as P

funlogDef :: P.LanguageDef st
funlogDef = haskellStyle
    { P.reservedNames = ["type", "fun", "begin", "end", "match", "with", "let", "in", "where", "operator"]
    , P.reservedOpNames = ["=", ":-", "->", "#", "_", "=>", ":"]
    , P.opStart = oneOf "*/%+-\\=<>!&|^;,$"
    , P.opLetter = oneOf ",:!#$%&*+./<=>?@\\^|-~"
    , P.identStart = lower
    }

opsTable :: [[Operator String () Identity Expr]]
opsTable = uncurry go <$> operators
  where
    go str AssociativityLeft = [Infix (opStarting str) AssocLeft]
    go str AssociativityRight = [Infix (opStarting str) AssocRight]
    go str AssociativityNone = [Infix (opStarting str) AssocNone]

P.TokenParser{..} = P.makeTokenParser funlogDef

parseProgram :: Parser Program
parseProgram = Program <$> (parseDecl `endBy` dot) <* eof

parseDecl :: Parser Decl
parseDecl = try parseTypeDecl <|> parseNormalDecl

parseNormalDecl :: Parser Decl
parseNormalDecl = do
    name <- bindable
    choice [ reservedOp "=" >> (DeclName name <$> parseExpr)
           , DeclSig name <$> (reservedOp ":" *> parseType) <?> "type signature"
           ]

parseTypeDecl :: Parser Decl
parseTypeDecl = do
    name <- identifier <* reservedOp "="
    between (reserved "type") (reserved "end") . fmap (DeclType name) $ parseTypeConstr `endBy1` dot

parseType :: Parser Type
parseType = chainr1 parseTypeNoFun (reservedOp "->" >> pure TyFun) <?> "type"

parseTypeNoFun :: Parser Type
parseTypeNoFun = chainl1 parseTypeSimple (pure TyApp) <?> "type without ->"

parseTypeSimple :: Parser Type
parseTypeSimple = TyConstr <$> identifier
                 <|> TyVar <$> variable
                 <|> parseTypeTuple

parseTypeTuple :: Parser Type
parseTypeTuple = fmap fix . parens $ parseType `sepBy` reservedOp "#"
  where
    fix [ty] = ty
    fix tys = TyTuple tys

variable :: Parser String
variable = lexeme ((:) <$> char '\'' <*> many1 (P.identLetter funlogDef)) <?> "variable"

parseTypeConstr :: Parser TypeConstr
parseTypeConstr = (TypeConstr <$> constructor <* colon <*> parseType) <?> "type constructor"

parseExpr :: Parser Expr
parseExpr = do
    expr <- buildExpressionParser opsTable parseCall
    option expr $ colon >> ExpType expr <$> parseType

parseCall :: Parser Expr
parseCall = ExpLit <$> integer <|> chainl1 parseTerm (pure ExpApp)

parseTerm :: Parser Expr
parseTerm = parens parseExprTuple
          <|> ExpName <$> bindable
          <|> ExpName <$> constructor
          <|> ExpLit <$> natural
          <|> parseLetExpr
          <|> parseMatchExpr
          <|> parseFunExpr
          <|> blockWhere "begin" parseExpr

parseExprTuple :: Parser Expr
parseExprTuple = fmap fix $ parseExpr `sepBy` reservedOp "#"
  where
    fix [ex] = ex
    fix exs = ExpTuple exs

parseLetExpr :: Parser Expr
parseLetExpr = do
    reserved "let"
    pat <- parsePattern
    reservedOp "="
    expr <- parseExpr
    reserved "in"
    ExpLet pat expr <$> parseExpr

parseMatchExpr :: Parser Expr
parseMatchExpr = do
    reserved "match"
    expr <- parseExpr
    reserved "with"
    pats <- parseFunClause `endBy1` dot
    reserved "end"
    pure $ ExpMatch expr pats

parseVarList :: Parser [Name]
parseVarList = braces $ many identifier

parseFunClause :: Parser Clause
parseFunClause = Clause <$> parseMultiPattern <* reserved "=>" <*> parseExpr <?> "pattern clause"

parseMultiPattern :: Parser MultiPattern
parseMultiPattern = many parsePattern

parsePattern :: Parser Pattern
parsePattern = eatWildcard *> pure PatWildcard
             <|> PatBind <$> bindable
             <|> flip PatData [] <$> constructor
             <|> PatLit <$> integer
             <|> parsePatternTuple

eatWildcard :: Parser ()
eatWildcard = flip label "wildcard" . void . lexeme . try $ char '_' >> many (P.identLetter funlogDef)

parsePatternTuple :: Parser Pattern
parsePatternTuple = fmap fix . parens $ parseComplexPattern `sepBy` reservedOp "#"
  where
    fix [pt] = pt
    fix pts = PatTuple pts

parseComplexPattern :: Parser Pattern
parseComplexPattern = do
    pat <- PatData <$> constructor <*> many parsePattern <|> parsePattern
    option pat $ reservedOp ":" >> PatType pat <$> parseType

bindable :: Parser String
bindable = identifier <|> reserved "operator" *> operator

constructor :: Parser String
constructor = lexeme $ (:) <$> upper <*> many (P.identLetter funlogDef)

parseWithWhere :: Parser Expr -> Parser Expr
parseWithWhere p = do
    e <- p
    option e $ ExpWhere e <$> block "where" (parseNormalDecl `endBy` dot)

parseFunExpr :: Parser Expr
parseFunExpr = blockWhere "fun" $ ExpFun <$> parseFunClause `endBy1` dot

block :: String -> Parser a -> Parser a
block intro = between (reserved intro) (reserved "end")

blockWhere :: String -> Parser Expr -> Parser Expr
blockWhere name = parseWithWhere . block name

{-# ANN opStarting "HLint: ignore Use String" #-}
opStarting :: [Char] -> Parser (Expr -> Expr -> Expr)
opStarting first = flip label ("operator starting with any of " ++ first) $ do
    op <- try $ lexeme $ (:) <$> oneOf first <*> many (P.opLetter funlogDef)
    pure $ ExpApp . ExpApp (ExpName op) 


{-|
Module      : BAE.Parser
Description : Parser of Boolean Arithmetic Expressions.
Maintainer  : fernandogamen@ciencias.unam.mx, pablog@ciencias.unam.mx

This source code was modified, but it is completely based on Fernando A. Galicia's implementation.
-}
module BAE.Parser
( Expr(..)
, UnaryOp(..)
, BinaryOp(..)
, RelationalOp(..)
, Type(..)
, TExpr(..)
, parseFile
) where

  import Control.Monad
  import System.IO
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Expr
  import Text.ParserCombinators.Parsec.Language
  import qualified Text.ParserCombinators.Parsec.Token as Token

  type Identifier = String

  data UnaryOp = Not | Succ | Pred | Alloc | Deref deriving (Show)

  data BinaryOp = And | Or | Add | Mul | Assig | Seq | App deriving (Show)

  data RelationalOp = Gt | Lt | Eq deriving (Show)

  data Expr = If Expr Expr Expr
            | Let Identifier Expr Expr
            | While Expr Expr
            | Fn Identifier Expr
            | RecurFn Identifier Identifier Expr
            | UnaryE UnaryOp Expr
            | BinaryE BinaryOp Expr Expr
            | RelationalE RelationalOp Expr Expr
            | V Identifier
            | I Integer
            | B Bool
            | Void deriving (Show)

  data Type = Integer | Boolean deriving (Show, Eq)

  data TExpr = Typed Expr Type deriving (Show)

  languageDef =
    emptyDef { Token.commentStart   = "/*"
             , Token.commentEnd     = "*/"
             , Token.commentLine    = "//"
             , Token.identStart     = letter
             , Token.identLetter    = alphaNum
             , Token.reservedNames  = ["if",
                                       "then",
                                       "else",
                                       "true",
                                       "false",
                                       "not",
                                       "succ",
                                       "pred",
                                       "let",
                                       "in",
                                       "end",
                                       "fn",
                                       "recurFn",
                                       "void",
                                       "alloc",
                                       "while",
                                       "do",
                                       "Integer",
                                       "Boolean"]
             , Token.reservedOpNames = ["+","*",">","<","=",":=","=>","$","&","|","not","!","::=",";","::"]}

  lexer = Token.makeTokenParser languageDef

  identifier = Token.identifier lexer
  reserved = Token.reserved lexer
  reservedOp = Token.reservedOp lexer
  parens = Token.parens lexer
  integer = Token.integer lexer
  whiteSpace = Token.whiteSpace lexer

  baeParser :: Parser TExpr
  baeParser = whiteSpace >> typed_expression

  typed_expression :: Parser TExpr
  typed_expression =
    do
      expr <- expression
      reservedOp "::"
      typo <- typoParser
      return $ Typed expr typo

  typoParser = (reserved "Integer" >> return Integer)
            <|> (reserved "Boolean" >> return Boolean)

  expression :: Parser Expr
  expression = ifExpr
            <|> letExpr
            <|> whileExpr
            <|> fnExpr
            <|> recurFnExpr
            <|> buildExpressionParser abOperators (parens expression
            <|> liftM I integer
            <|> liftM V identifier
            <|> (reserved "void" >> return (Void))
            <|> (reserved "true" >> return (B True))
            <|> (reserved "false" >> return (B False)))

  ifExpr :: Parser Expr
  ifExpr =
    do reserved "if"
       cond <- expression
       reserved "then"
       expr1 <- expression
       reserved "else"
       expr2 <- expression
       return $ If cond expr1 expr2

  letExpr :: Parser Expr
  letExpr =
    do reserved "let"
       var <- identifier
       reserved ":="
       expr1 <- expression
       reserved "in"
       expr2 <- expression
       reserved "end"
       return $ Let var expr1 expr2

  whileExpr :: Parser Expr
  whileExpr =
    do reserved "while"
       expr1 <- expression
       reserved "do"
       expr2 <- expression
       reserved "end"
       return $ While expr1 expr2

  fnExpr :: Parser Expr
  fnExpr =
    do reserved "fn"
       var <- identifier
       reserved "=>"
       expr <- expression
       return $ Fn var expr

  recurFnExpr :: Parser Expr
  recurFnExpr =
    do reserved "recurFn"
       varf <- identifier
       varx <- identifier
       reserved "=>"
       expr <- expression
       return $ RecurFn varf varx expr

  abOperators = [[Prefix (reservedOp "alloc" >> return (UnaryE Alloc)),
                  Prefix (reservedOp "!" >> return (UnaryE Deref))]
                ,[Prefix (reservedOp "not" >> return (UnaryE Not))]
                ,[Prefix (reservedOp "succ" >> return (UnaryE Succ)),
                  Prefix (reservedOp "pred" >> return (UnaryE Pred))]
                ,[Infix (reservedOp "$" >> return (BinaryE App)) AssocLeft]
                ,[Infix (reservedOp "&" >> return (BinaryE And)) AssocLeft,
                  Infix (reservedOp "|" >> return (BinaryE Or)) AssocLeft]
                ,[Infix (reservedOp "*" >> return (BinaryE Mul)) AssocLeft]
                ,[Infix (reservedOp "+" >> return (BinaryE Add)) AssocLeft]
                ,[Infix (reservedOp ">" >> return (RelationalE Gt)) AssocLeft]
                ,[Infix (reservedOp "<" >> return (RelationalE Lt)) AssocLeft]
                ,[Infix (reservedOp "=" >> return (RelationalE Eq)) AssocLeft]
                ,[Infix (reservedOp "::=" >> return (BinaryE Assig)) AssocLeft]
                ,[Infix (reservedOp ";" >> return (BinaryE Seq)) AssocLeft]]

  parseString :: String -> TExpr
  parseString str =
    case parse baeParser "" str of
      Left e -> error $ show e
      Right r -> r

  parseFile :: String -> IO TExpr
  parseFile file =
    do program <- readFile file
       case parse baeParser "" program of
        Left e -> print e >> fail "Parsing error."
        Right r -> return r

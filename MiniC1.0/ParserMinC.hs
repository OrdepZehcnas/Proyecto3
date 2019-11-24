{-|
Module      : ParserMinC
Description : An parser for the concrete syntax of Mini C programs.
Copyright   : (c) Fernando Abigail Galicia-Mendoza, 2019
License     : GPL-3
Maintainer  : fernandogamen@ciencias.unam.mx,
              pablog@ciencias.unam.mx
Stability   : experimental-education
Portability : POSIX

This script is an implementation of a parser for the Mini C programs.
This script was created by following the tutorial: https://wiki.haskell.org/Parsing_a_simple_imperative_language
-}
module ParserMinC where

  import System.IO
  import Control.Monad
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Expr
  import Text.ParserCombinators.Parsec.Language
  import qualified Text.ParserCombinators.Parsec.Token as Token

  -- | Type that represents the set of possible variable names.
  type Identifier = String

  data Stmt = Var Identifier -- ^ Constructor for the concrete syntax of variables.
            | FunS [Identifier] Stmt -- ^ Constructor for the concrete syntax of lambda terms.
            | AppS Stmt Stmt -- ^ Constructor for the concrete syntax of applications.
            | LetES Identifier Stmt Stmt -- ^ Constructor for the concrete syntax of the let statements.
            | FunFS Identifier [Identifier] Stmt -- ^ Constructor for the concrete syntax of the function with name.
            | Num Integer -- ^ Constructor for the concrete syntax of numbers.
            | Boolean Bool -- ^ Constructor for the concrete syntax of booleans.
            | SucS Stmt -- ^ Constructor for the concrete syntax of the successor operator.
            | PredS Stmt -- ^ Constructor for the concrete syntax of the predecessor operator.
            | NegS Stmt -- ^ Constructor for the concrete syntax of the negation operator.
            | PlusS Stmt Stmt -- ^ Constructor for the concrete syntax of the plus operator.
            | ProdS Stmt Stmt -- ^ Constructor for the concrete syntax of the product operator.
            | ConjS Stmt Stmt -- ^ Constructor for the concrete syntax of the conjunction operator.
            | DisyS Stmt Stmt -- ^ Constructor for the concrete syntax of the disjunction operator.
            | EquiS Stmt Stmt -- ^ Constructor for the concrete syntax of the equality operator.
            | GtS Stmt Stmt -- ^ Constructor for the concrete syntax of the greater than operator.
            | LtS Stmt Stmt -- ^ Constructor for the concrete syntax of the lower than operator.
            | IftS Stmt Stmt Stmt -- ^ Constructor for the concrete syntax of the if-else conditional operator.
            | SeqS [Stmt]
            | AssignS Stmt Stmt
            | RefS Stmt
            | DeRefS Stmt
            | LiS Int
            | WhileS Stmt Stmt
            | VoidS
  
  instance Show Stmt where
    show l = case l of
      Var x -> x
      Num n -> show n
      Boolean b -> show b
      SucS e-> "suc("++show e++")"
      PredS e-> "pred("++show e++")"
      PlusS e1 e2-> "("++show e1++"+"++show e2++")"
      ProdS e1 e2-> "("++show e1++"*"++show e2++")"
      NegS e-> "not("++show e++")"
      ConjS e1 e2-> "("++show e1++"&&"++show e2++")"
      DisyS e1 e2-> "("++show e1++"||"++show e2++")"
      GtS e1 e2-> "("++show e1++">"++show e2++")"
      LtS e1 e2-> "("++show e1++"<"++show e2++")"
      EquiS e1 e2-> "("++show e1++"=="++show e2++")"
      IftS e1 e2 e3-> "if "++show e1++" then "++show e2++" else "++show e3
      FunS xs t -> "(fun "++show xs++" => "++show t++")"
      FunFS f xs t -> "(funf "++f++" "++show xs++" => \n"++show t++")"
      LetES x e1 e2 -> "(let "++x++"="++show e1++" in "++show e2++")"
      AppS t s -> "("++show t++" <+> "++show s++")"
      --
      SeqS es -> showSeqs es
      LiS x -> "(l_"++show x++")"
      AssignS e1 e2 -> show e1++" := "++show e2
      RefS e -> "ref ("++show e++")"
      DeRefS e -> "!("++show e++")"
      WhileS e1 e2 -> "while "++show e1++"{\n"++show e2++"}"

  showSeqs :: [Stmt] -> String
  showSeqs [] = ""
  showSeqs [s] = show s
  showSeqs (s:ss) = show s++";\n"++showSeqs ss

  -- A dictionary for our language.
  languageDef = 
    emptyDef { Token.commentStart   = "/*"
             , Token.commentEnd     = "*/"
             , Token.commentLine    = "//"
             , Token.identStart     = letter
             , Token.identLetter    = alphaNum
             , Token.reservedNames  = ["if","then","else","true","false","not","and","or",
                                       "suc","pred","fun","->","=","let","funf","=>","in",
                                       "while","do","end","ref","l","()"]
             , Token.reservedOpNames = ["<+>","+","*",">","<","==",":=","and","or","not","::","=>",
                                        ":=","!","()"]}

  -- The lexer reimplementation.
  lexer = Token.makeTokenParser languageDef

  -- Elements that need to be tokenizen.
  identifier = Token.identifier lexer
  reserved = Token.reserved lexer
  reservedOp = Token.reservedOp lexer
  parens = Token.parens lexer
  braces = Token.braces lexer
  integer = Token.integer lexer
  whiteSpace = Token.whiteSpace lexer
  semi       = Token.semi       lexer

  -- | The lamParser function constructs the expression from tokens.
  lamParser :: Parser Stmt
  lamParser = whiteSpace >> statement

  statement :: Parser Stmt
  statement = seqStmt

  -- | The 'statement' function constructs the expression from tokens.
  statement' :: Parser Stmt
  statement' = letStmt 
             <|> funStmt
             <|> letFunStmt
             <|> ifStmt
             <|> whileStmt
             <|> buildExpressionParser appOp (parens statement'
             <|> liftM Var identifier
             <|> liftM Num integer
             <|> (reserved "true" >> return (Boolean True))
             <|> (reserved "false" >> return (Boolean False)))

  seqStmt :: Parser Stmt
  seqStmt =
    do stmts <- (statement' `sepBy` semi)
       return $ if length stmts == 1 then head stmts else SeqS stmts

  whileStmt :: Parser Stmt
  whileStmt =
    do reserved "while"
       cond <- statement
       stmt1 <- braces statement
       return $ WhileS cond stmt1

  -- | The 'funStmt' function try to create a anonymous function statement.
  funStmt :: Parser Stmt
  funStmt = 
    do reserved "fun"
       char '['
       args <- identifier `sepBy` (char ',')
       char ']'
       char ' '
       reserved "=>"
       stmt <- statement
       return $ FunS args stmt

  -- | The 'letStmt' function try to create a let statement.
  letStmt :: Parser Stmt
  letStmt =
    do reserved "let"
       var <- identifier
       reserved "="
       stmt1 <- statement
       reserved "in"
       stmt2 <- statement
       reserved "end"
       return $ LetES var stmt1 stmt2

  -- | The 'letFunStmt' function try to create a function with name statement.
  letFunStmt :: Parser Stmt
  letFunStmt = 
    do reserved "funf"
       nameF <- identifier
       char '['
       args <- identifier `sepBy` (char ',')
       char ']'
       char ' '
       reserved "=>"
       stmt <- statement
       return $ FunFS nameF args stmt

  -- | The 'ifStmt' function try to create a if-else statement.
  ifStmt :: Parser Stmt
  ifStmt =
    do reserved "if"
       cond <- statement
       reserved "then"
       stmt1 <- statement
       reserved "else"
       stmt2 <- statement
       return $ IftS cond stmt1 stmt2

  -- | A dictionary for the operators.
  appOp = [[Prefix (reservedOp "ref" >> return RefS),
            Prefix (reservedOp "!" >> return DeRefS)],
           [Prefix (reservedOp "not" >> return NegS)],
           [Prefix (reservedOp "suc" >> return SucS),
            Prefix (reservedOp "pred" >> return PredS)],
           [Infix (reservedOp "<+>" >> return AppS) AssocLeft],
           [Infix (reservedOp "and" >> return ConjS) AssocLeft,
            Infix (reservedOp "or" >> return DisyS) AssocLeft],
           [Infix (reservedOp "*" >> return ProdS) AssocLeft],
           [Infix (reservedOp "+" >> return PlusS) AssocLeft],
           [Infix (reservedOp ">" >> return GtS) AssocLeft],
           [Infix (reservedOp "<" >> return LtS) AssocLeft],
           [Infix (reservedOp "==" >> return EquiS) AssocLeft],
           [Infix (reservedOp ":=" >> return AssignS) AssocRight]]

  -- | The parseString parses an string and returns (in case to be possible) the abstract
  -- syntax of a lambda term.
  parseString :: String -> Stmt
  parseString str =
    case parse lamParser "" str of
      Left e -> error $ show e
      Right r -> r

  -- | The parseFile is an extension to files of the parseString function.
  parseFile :: String -> IO Stmt
  parseFile file =
    do program <- readFile file
       case parse lamParser "" program of 
        Left e -> print e >> fail "Parsing error."
        Right r -> return r
module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Part 2

-- Data type for arithmetic expressions
data Aexp
  = VarAexp String         -- Variable
  | NumAexp Int            -- Integer constant
  | AddAexp Aexp Aexp      -- Addition
  | SubAexp Aexp Aexp      -- Subtraction
  | MulAexp Aexp Aexp      -- Multiplication
  deriving Show

-- Data type for boolean expressions
data Bexp
  = BoolConst Bool        -- Boolean constant False
  | IntEq Aexp Aexp       -- Equality
  | IntIneq Aexp Aexp     -- Inequality
  | BoolEq Bexp Bexp      -- Boolean Equality
  | BoolAnd Bexp Bexp     -- Boolean And
  | Not Bexp              -- Negation
  deriving Show

-- Data type for statements
data Stm
  = Assign String Aexp       -- Assignment: x := a
  | Seq [Stm]                -- Sequence: various instructions
  | If Bexp Stm Stm          -- Conditional: if b then instr1 else instr2
  | While Bexp Stm           -- Loop: while b do instr
  | Skip                     -- Skip: Does Nothing - Just a useful statement
  deriving Show

type Program = [Stm]

-- Defines the language syntax
languageDef =
   emptyDef {
              Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "if"
                                      , "then"
                                      , "else"
                                      , "while"
                                      , "do"
                                      , "True"
                                      , "False"
                                      , "not"
                                      , "and"
                                      ]
            , Token.reservedOpNames = ["+", "-", "*", ":="
                                      , "<=", "and", "not"
                                      ]
            }

-- Lexer for the language
lexer = Token.makeTokenParser languageDef

-- Parsers for different components
identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

-- Top-level parser for the language
whileParser :: Parser Stm
whileParser = whiteSpace >> statement

-- Parser for the statements
statement :: Parser Stm
statement =   parens statement
          <|> sequenceOfStm

-- Parser for sequences of statements
sequenceOfStm =
  do list <- sepBy statement' semi
     return $ if length list == 1 then head list else Seq list

-- Parser for a statement
statement' :: Parser Stm
statement' =   ifStm
           <|> whileStm
           <|> assignStm
           <|> (try (lookAhead (string "else")) >> return Skip)
           <|> (try (string "" >> notFollowedBy alphaNum) >> return Skip)

-- Parser for If statements
ifStm :: Parser Stm
ifStm = do
  reserved "if"
  cond <- bExpression
  reserved "then"
  stmt1 <- statement
  reserved "else"
  stmt2 <- ifElseParser
  return $ If cond stmt1 stmt2

-- Parser for optional Else clause in If statements
ifElseParser :: Parser Stm
ifElseParser = whileStm <|> ifStm <|> assignStm <|> parens statement


-- Parser for While statements
whileStm :: Parser Stm
whileStm =
  do reserved "while"
     cond <- bExpression
     reserved "do"
     stmt <- statement
     return $ While cond stmt

-- Parser for Assignment statements
assignStm :: Parser Stm
assignStm =
  do var  <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Assign var expr

-- Parser for Arithmetic expressions
aExpression :: Parser Aexp
aExpression = buildExpressionParser aOperators aTerm

-- Parser for Boolean expressions
bExpression :: Parser Bexp
bExpression = buildExpressionParser bOperators bTerm

-- Operator precedence and associativity for Arithmetic expressions
aOperators = [ [Infix  (reservedOp "*"   >> return MulAexp) AssocLeft]
             , [Infix  (reservedOp "+"   >> return AddAexp) AssocLeft,
                Infix  (reservedOp "-"   >> return SubAexp) AssocLeft]
              ]

-- Operator precedence and associativity for Boolean expressions
bOperators = [ [Prefix (reservedOp "not" >> return Not )          ]
              , [Infix  (reservedOp "="  >> return BoolEq) AssocLeft]
             , [Infix  (reservedOp "and" >> return BoolAnd) AssocLeft]
             ]

-- Parser for atomic Arithmetic expressions
aTerm =  parens aExpression
     <|> liftM VarAexp identifier
     <|> liftM (NumAexp . fromIntegral) integer

-- Parser for atomic Boolean expressions
bTerm =  parens bExpression
     <|> (reserved "True"  >> return (BoolConst True))
     <|> (reserved "False" >> return (BoolConst False))
     <|> try intEqExpression
     <|> try intIneqExpression

-- Parser for Addition expressions
addExpression :: Parser Aexp
addExpression =
  do a1 <- aExpression
     reservedOp "+"
     a2 <- aExpression
     return $ AddAexp a1 a2

-- Parser for Subtraction expressions
subExpression :: Parser Aexp
subExpression =
  do a1 <- aExpression
     reservedOp "-"
     a2 <- aExpression
     return $ SubAexp a1 a2

-- Parser for Multiplication expressions
mulExpression :: Parser Aexp
mulExpression =
  do a1 <- aExpression
     reservedOp "*"
     a2 <- aExpression
     return $ MulAexp a1 a2

-- Parser for Integer Equality expressions
intEqExpression :: Parser Bexp
intEqExpression = do
  a1 <- aExpression
  reservedOp "=="
  a2 <- aExpression
  return $ IntEq a1 a2

-- Parser for Integer Inequality expressions
intIneqExpression :: Parser Bexp
intIneqExpression = do
  a1 <- aExpression
  reservedOp "<="
  a2 <- aExpression
  return $ IntIneq a1 a2

-- Entry point for parsing a program
parse :: String -> Program
parse str =
  case Text.ParserCombinators.Parsec.parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> [r]
-- expr ::= term + expr | term - expr | term

-- term ::= factor*term | factor

-- factor ::= (expr) | int | string

-- att ::= string := expr;

-- boolExp ::= expr == expr | (expr == expr) | expr <= expr | (expr <= expr) | not boolExp | not (boolExp) | 
--             boolExp = boolExp | (boolExp = boolExp) | boolExp and boolexp | (boolExp and boolExp)

-- inst := att;inst | att;

-- whi ::= while (boolExp) do att | while (boolExp) do (inst)

-- if ::= if boolExp then att; | if boolExp then (inst) | if boolExp then att;else att |
--        if boolExp then (inst) else (inst)

-- S ::= A S | A

-- A ::= inst | whi | if

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import GHC.Plugins (all2)

data Aexp
  = VarAexp String         -- Variable
  | NumAexp Int            -- Integer constant
  | AddAexp Aexp Aexp      -- Addition
  | SubAexp Aexp Aexp      -- Subtraction
  | MulAexp Aexp Aexp      -- Multiplication
  deriving Show

data Bexp
  = BoolConst Bool          -- Boolean constant False
  | IntEq Aexp Aexp       -- Equality
  | IntIneq Aexp Aexp     -- Inequality
  | BoolEq Bexp Bexp      -- Boolean Equality
  | And Bexp Bexp         -- Boolean And
  | Not Bexp              -- Negation
  deriving Show

data Stm
  = Assign String Aexp       -- Assignment: x := a
  | Seq [Stm]                -- Sequence: various instructions
  | If Bexp Stm Stm          -- Conditional: if b then instr1 else instr2
  | While Bexp Stm           -- Loop: while b do instr
  | Skip
  deriving Show

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

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

whileParser :: Parser Stm
whileParser = whiteSpace >> statement

statement :: Parser Stm
statement =   parens statement
          <|> sequenceOfStm

sequenceOfStm =
  do list <- sepBy statement' semi
     return $ if length list == 1 then head list else Seq list


statement' :: Parser Stm
statement' =   ifStm
           <|> whileStm
           <|> assignStm
           <|> (try (string "" >> notFollowedBy alphaNum) >> return Skip)

ifStm :: Parser Stm
ifStm = do
  reserved "if"
  cond <- bExpression
  reserved "then"
  stmt1 <- statement
  optionalElse <- optionMaybe (reserved "else" >> statement)
  case optionalElse of
    Just stmt2 -> return $ If cond stmt1 stmt2
    Nothing    -> return $ If cond stmt1 Skip



whileStm :: Parser Stm
whileStm =
  do reserved "while"
     cond <- bExpression
     reserved "do"
     stmt <- statement
     return $ While cond stmt

assignStm :: Parser Stm
assignStm =
  do var  <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Assign var expr

aExpression :: Parser Aexp
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser Bexp
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Infix  (reservedOp "*"   >> return MulAexp) AssocLeft]
             , [Infix  (reservedOp "+"   >> return AddAexp) AssocLeft,
                Infix  (reservedOp "-"   >> return SubAexp) AssocLeft]
              ]

bOperators = [ [Prefix (reservedOp "not" >> return Not )          ]
             , [Infix  (reservedOp "and" >> return And) AssocLeft]
             , [Infix  (reservedOp "="  >> return BoolEq) AssocLeft]
             ]


aTerm =  parens aExpression
     <|> liftM VarAexp identifier
     <|> liftM (NumAexp . fromIntegral) integer

bTerm =  parens bExpression
     <|> (reserved "True"  >> return (BoolConst True))
     <|> (reserved "False" >> return (BoolConst False))
     <|> try intEqExpression
     <|> try intIneqExpression

-- Addition expression
addExpression :: Parser Aexp
addExpression =
  do a1 <- aExpression
     reservedOp "+"
     a2 <- aExpression
     return $ AddAexp a1 a2

-- Subtraction expression
subExpression :: Parser Aexp
subExpression =
  do a1 <- aExpression
     reservedOp "-"
     a2 <- aExpression
     return $ SubAexp a1 a2

-- Multiplication expression
mulExpression :: Parser Aexp
mulExpression =
  do a1 <- aExpression
     reservedOp "*"
     a2 <- aExpression
     return $ MulAexp a1 a2

intEqExpression :: Parser Bexp
intEqExpression = do
  a1 <- aExpression
  reservedOp "=="
  a2 <- aExpression
  return $ IntEq a1 a2

intIneqExpression :: Parser Bexp
intIneqExpression = do
  a1 <- aExpression
  reservedOp "<="
  a2 <- aExpression
  return $ IntIneq a1 a2



parseString :: String -> Stm
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Stm
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r
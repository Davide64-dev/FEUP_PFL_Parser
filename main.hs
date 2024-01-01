
import Data.List (intersperse, sortBy, splitAt)
import Data.Char (isSpace, isDigit, isAlpha)
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import GHC.Plugins (all2)


-- PFL 2023/24 - Haskell practical assignment quickstart

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data NumberOrBool = N Int | B Bool
  deriving (Show, Eq)

type Stack = [NumberOrBool]

type State = [(String, NumberOrBool)]

createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [x] = getValue x
  where
    getValue (N myInt) = show myInt
    getValue (B myBool) = show myBool
stack2Str (x:xs) = getValue x ++ "," ++ stack2Str xs
  where
    getValue (N myInt) = show myInt
    getValue (B myBool) = show myBool

createEmptyState :: Main.State
createEmptyState = []

state2Str :: Main.State -> String
state2Str state =
  case sortedState of
    [] -> ""
    _  -> init $ concatMap (\(k, v) -> k ++ "=" ++ getValue v ++ ",") sortedState
  where
    sortedState = sortBy (\(k1, _) (k2, _) -> compare k1 k2) state
    getValue (N n) = show n
    getValue (B b) = show b

findValueState :: Main.State -> String -> NumberOrBool
findValueState [] _ = error "Key not found in state"
findValueState ((k, v):remainState) key
    | k == key  = v
    | otherwise = findValueState remainState key

checkKeyExists :: Main.State -> String -> Bool

checkKeyExists [] _ = False
checkKeyExists ((k, v):remainState) key
    | k == key  = True
    | otherwise = checkKeyExists remainState key


addValueState :: Main.State -> String -> NumberOrBool -> Main.State

addValueState state key value = (key, value) : state

replaceValueState :: Main.State -> String -> NumberOrBool -> Main.State
replaceValueState state key value = map updateEntry state
  where
    updateEntry (existingKey, existingValue)
      | existingKey == key = (existingKey, value)
      | otherwise = (existingKey, existingValue)

addOrUpdateValue :: Main.State -> String -> NumberOrBool -> Main.State

addOrUpdateValue state key value
    | checkKeyExists state key = replaceValueState state key value
    | otherwise = addValueState state key value


run :: (Code, Stack, Main.State) -> (Code, Stack, Main.State)
run ([], stack, state) = ([], stack, state)

run (Add: remainCode, N firstElem : N secondElem : remainStack, state) = run(remainCode, N (firstElem + secondElem) : remainStack, state)

run (Mult : remainCode, N firstElem : N secondElem : remainStack, state) = run(remainCode, N (firstElem * secondElem) : remainStack, state)

run (Sub : remainCode, N firstElem : N secondElem : remainStack, state) = run(remainCode, N (firstElem - secondElem) : remainStack, state)

run (Equ : remainCode, firstElem : secondElem : remainStack, state) = run(remainCode, B (firstElem == secondElem) : remainStack, state)

run (And : remainCode, B firstElem :B secondElem : remainStack, state) = run(remainCode, B (firstElem && secondElem) : remainStack, state)

run (Le : remainCode, N firstElem : N secondElem : remainStack, state) = run (remainCode, B (firstElem <= secondElem) : remainStack, state)

run (Push n : remainCode, stack, state) = run (remainCode, N (fromIntegral n) : stack, state)

run (Tru : remainCode, stack, state) = run(remainCode, B True : stack, state)

run (Fals : remainCode, stack, state) = run(remainCode, B False : stack, state)

run (Fetch x : remainCode, stack, state) = run(remainCode, findValueState state x : stack, state)

run (Store x : remainCode, firstElem : remainStack, state) = run(remainCode, remainStack, addOrUpdateValue state x firstElem) 

run (Neg : remainCode, B firstElem : remainStack, state) = run(remainCode, B (not firstElem) : remainStack, state)

run (Noop : remainCode, stack, state) = run(remainCode, stack, state)

run (Branch c1 c2 : remainCode, B True : stack, state) = run(c1 ++ remainCode, stack, state)

run (Branch c1 c2 : remainCode, B False : stack, state) = run(c2 ++ remainCode, stack, state)

run (Loop c1 c2 : remainCode, stack, state) = run(c1 ++  [Branch (c2 ++ [Loop c1 c2]) [Noop]], stack, state)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

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
  | BoolAnd Bexp Bexp         -- Boolean And
  | Not Bexp              -- Negation
  deriving Show

data Stm
  = Assign String Aexp       -- Assignment: x := a
  | Seq [Stm]                -- Sequence: various instructions
  | If Bexp Stm Stm          -- Conditional: if b then instr1 else instr2
  | While Bexp Stm           -- Loop: while b do instr
  | Skip
  deriving Show

type Program = [Stm]

compA :: Aexp -> Code
compA (VarAexp var) = [Fetch var]
compA (NumAexp num) = [Push (fromIntegral num)]
compA (AddAexp v1 v2) = compA v1 ++ compA v2 ++ [Add]
compA (SubAexp v1 v2) = compA v2 ++ compA v1 ++ [Sub]
compA (MulAexp v1 v2) = compA v1 ++ compA v2 ++ [Mult]

compB :: Bexp -> Code
compB (BoolConst True) = [Tru]
compB (BoolConst False) = [Fals]
compB (IntEq v1 v2) = compA v1 ++ compA v2 ++ [Equ]
compB (IntIneq v1 v2) = compA v2 ++ compA v1 ++ [Le]
compB (BoolEq b1 b2) = compB b1 ++ compB b2 ++ [Equ]
compB (BoolAnd b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (Not b) = compB b ++ [Neg]

compStm :: Stm -> Code
compStm (Assign var aexp) = compA aexp ++ [Store var]
compStm (Seq stmts) = concatMap compStm stmts  -- Process all statements in the sequence
compStm (If bexp s1 s2) = compB bexp ++ [Branch (compStm s1) (compStm s2)]
compStm (While bexp body) = [Loop (compB bexp) (compStm body)]
compStm Skip = []

compile :: Program -> Code
compile = concatMap compStm


parse :: String -> Program
parse str =
  case Text.ParserCombinators.Parsec.parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> [r]  -- Wrap the result in a list to form a Program


-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (compile (Main.parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

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
           <|> (try (lookAhead (string "else")) >> return Skip)
           <|> (try (string "" >> notFollowedBy alphaNum) >> return Skip)


ifStm :: Parser Stm
ifStm = do
  reserved "if"
  cond <- bExpression
  reserved "then"
  stmt1 <- statement
  reserved "else"
  stmt2 <- ifElseParser
  return $ If cond stmt1 stmt2


ifElseParser :: Parser Stm
ifElseParser = whileStm <|> ifStm <|> assignStm <|> parens statement



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
              , [Infix  (reservedOp "="  >> return BoolEq) AssocLeft]
             , [Infix  (reservedOp "and" >> return BoolAnd) AssocLeft]
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

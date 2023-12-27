import Data.List (intersperse, sortBy)

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

createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state =
  case sortedState of
    [] -> ""
    _  -> init $ concatMap (\(k, v) -> k ++ "=" ++ getValue v ++ ",") sortedState
  where
    sortedState = sortBy (\(k1, _) (k2, _) -> compare k1 k2) state
    getValue (N n) = show n
    getValue (B b) = show b

findValueState :: State -> String -> NumberOrBool
findValueState [] _ = error "Key not found in state"
findValueState ((k, v):remainState) key
    | k == key  = v
    | otherwise = findValueState remainState key

checkKeyExists :: State -> String -> Bool

checkKeyExists [] _ = False
checkKeyExists ((k, v):remainState) key
    | k == key  = True
    | otherwise = checkKeyExists remainState key

-- add the case where the value is already in the state
addValueState :: State -> String -> NumberOrBool -> State

addValueState state key value = (key, value) : state

replaceValueState :: State -> String -> NumberOrBool -> State
replaceValueState state key value = map updateEntry state
  where
    updateEntry (existingKey, existingValue)
      | existingKey == key = (existingKey, value)
      | otherwise = (existingKey, existingValue)

addOrUpdateValue :: State -> String -> NumberOrBool -> State

addOrUpdateValue state key value
    | checkKeyExists state key = replaceValueState state key value
    | otherwise = addValueState state key value


run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)

run (Add: remainCode, N firstElem : N secondElem : remainStack, state) = run(remainCode, N (firstElem + secondElem) : remainStack, state)

run (Mult : remainCode, N firstElem : N secondElem : remainStack, state) = run(remainCode, N (firstElem * secondElem) : remainStack, state)

run (Sub : remainCode, N firstElem : N secondElem : remainStack, state) = run(remainCode, N (firstElem - secondElem) : remainStack, state)

run (Equ : remainCode, firstElem : secondElem : remainStack, state) = run(remainCode, B (firstElem == secondElem) : remainStack, state)

run (Le : remainCode, N firstElem : N secondElem : remainStack, state) = run (remainCode, B (firstElem <= secondElem) : remainStack, state)

run (Push n : remainCode, stack, state) = run (remainCode, N (fromIntegral n) : stack, state)

run (Tru : remainCode, stack, state) = run(remainCode, B True : stack, state)

run (Fals : remainCode, stack, state) = run(remainCode, B False : stack, state)

run (Fetch x : remainCode, stack, state) = run(remainCode, findValueState state x : stack, state)

run (Store x : remainCode, firstElem : remainStack, state) = run(remainCode, remainStack, addOrUpdateValue state x firstElem) 

run (Neg : remainCode, B firstElem : remainStack, state) = run(remainCode, B (not firstElem) : remainStack, state)

run (Noop : remainCode, stack, state) = run(remainCode, stack, state)

run (Branch c1 c2 : remainCode, B True : stack, state) = run(c1, stack, state)

run (Branch c1 c2 : remainCode, B False : stack, state) = run(c2, stack, state)

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
  = TrueBexp              -- Boolean constant True
  | FalseBexp             -- Boolean constant False
  | Eq Aexp Aexp          -- Equality
  | Not Bexp              -- Negation
  deriving Show

data Stm
  = Assign String Aexp       -- Assignment: x := a
  | Seq Stm Stm              -- Sequence: instr1 ; instr2
  | If Bexp Stm Stm          -- Conditional: if b then instr1 else instr2
  | While Bexp Stm           -- Loop: while b do instr
  deriving Show

type Program = [Stm]

compA :: Aexp -> Code
compA (VarAexp var) = [Fetch var]

compA (NumAexp num) = [Push (fromIntegral num)]

compA (AddAexp v1 v2) = compA v1 ++ compA v2 ++ [Add]

compA (SubAexp v1 v2) = compA v1 ++ compA v2 ++ [Sub]

compA (MulAexp v1 v2) = compA v1 ++ compA v2 ++ [Mult]


compB :: Bexp -> Code
compB TrueBexp = [Tru]

compB FalseBexp = [Fals]

compB (Eq v1 v2) = compA v1 ++ compA v2 ++ [Equ]

compB (Not b) = compB b ++ [Neg]


compStm :: Stm -> Code
compStm (Assign var inst) = compA inst ++ [Store var]

compStm (Seq inst1 inst2) = compStm inst1 ++ compStm inst2

compStm (If bexp inst1 inst2) = compB bexp ++ [Branch (compStm inst1) (compStm inst2)]

compStm (While bexp inst) = [Loop (compB bexp) (compStm inst)]

compile :: Program -> Code
compile = concatMap compStm 

parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

main :: IO ()
main = do
  let myState = [("A", N 42), ("C", B True), ("A", N 30)]
  putStrLn $ "State as String: " ++ state2Str myState

  let stack1 = [N 42, B True, N (-3)]
  let stack2 = [B False, N 5, B True]

  putStrLn $ "Stack 1: " ++ stack2Str stack1
  putStrLn $ "Stack 2: " ++ stack2Str stack2
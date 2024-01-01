module Assembler where

import Data.List (intersperse, sortBy, splitAt)
import Data.Char (isSpace, isDigit, isAlpha)

-- Part 1

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

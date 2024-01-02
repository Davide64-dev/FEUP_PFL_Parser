module Compiler where 

import Parser
import Assembler

-- Part 2

-- Compiles arithmetic expressions into assembly code
compA :: Aexp -> Code
compA (VarAexp var) = [Fetch var]
compA (NumAexp num) = [Push (fromIntegral num)]
compA (AddAexp v1 v2) = compA v1 ++ compA v2 ++ [Add]
compA (SubAexp v1 v2) = compA v2 ++ compA v1 ++ [Sub]
compA (MulAexp v1 v2) = compA v1 ++ compA v2 ++ [Mult]

-- Compiles boolean expressions into assembly code
compB :: Bexp -> Code
compB (BoolConst True) = [Tru]
compB (BoolConst False) = [Fals]
compB (IntEq v1 v2) = compA v1 ++ compA v2 ++ [Equ]
compB (IntIneq v1 v2) = compA v2 ++ compA v1 ++ [Le]
compB (BoolEq b1 b2) = compB b1 ++ compB b2 ++ [Equ]
compB (BoolAnd b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (Not b) = compB b ++ [Neg]

-- Compiles statements into assembly code
compStm :: Stm -> Code
compStm (Assign var aexp) = compA aexp ++ [Store var]
compStm (Seq stmts) = concatMap compStm stmts
compStm (If bexp s1 s2) = compB bexp ++ [Branch (compStm s1) (compStm s2)]
compStm (While bexp body) = [Loop (compB bexp) (compStm body)]
compStm Skip = []

-- Compiles a program into assembly code
compile :: Program -> Code
compile = concatMap compStm
module Compiler where 

import Parser
import Assembler

-- Part 2

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
compStm (Seq stmts) = concatMap compStm stmts
compStm (If bexp s1 s2) = compB bexp ++ [Branch (compStm s1) (compStm s2)]
compStm (While bexp body) = [Loop (compB bexp) (compStm body)]
compStm Skip = []

compile :: Program -> Code
compile = concatMap compStm
import Assembler
import Compiler
import Parser


-- PFL 2023/24 - Haskell practical assignment quickstart


testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)

-- Assembler Examples:
test1 = testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
test2 = testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
test3 = testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
test4 = testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
test5 = testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
test6 = testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
test7 = testAssembler [Push (-20),Push (-21), Le] == ("True","")
test8 = testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
test9 = testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (compile (Parser.parse programCode), createEmptyStack, createEmptyState)

-- Parser Examples:
test10 = testParser "x := 5; x := x - 1;" == ("","x=4")
test11 = testParser "x := 0 - 2;" == ("","x=-2")
test12 = testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
test13 = testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
test14 = testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
test15 = testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
test16 = testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
test17 = testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
test18 = testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
test19 = testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
test20 = testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
test21 = testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")



-- For Code Testing
main :: IO ()
main = do
  putStrLn "Starting the Assembler tests..."
  putStrLn $ "Test 1: " ++ show test1
  putStrLn $ "Test 2: " ++ show test2
  putStrLn $ "Test 3: " ++ show test3
  putStrLn $ "Test 4: " ++ show test4
  putStrLn $ "Test 5: " ++ show test5
  putStrLn $ "Test 6: " ++ show test6
  putStrLn $ "Test 7: " ++ show test7
  putStrLn $ "Test 8: " ++ show test8
  putStrLn $ "Test 9: " ++ show test9
  putStrLn "Starting the Parser tests..."
  putStrLn $ "Test 10: " ++ show test10
  putStrLn $ "Test 11: " ++ show test11
  putStrLn $ "Test 12: " ++ show test12
  putStrLn $ "Test 13: " ++ show test13
  putStrLn $ "Test 14: " ++ show test14
  putStrLn $ "Test 15: " ++ show test15
  putStrLn $ "Test 16: " ++ show test16
  putStrLn $ "Test 17: " ++ show test17
  putStrLn $ "Test 18: " ++ show test18
  putStrLn $ "Test 19: " ++ show test19
  putStrLn $ "Test 20: " ++ show test20
  putStrLn $ "Test 21: " ++ show test21

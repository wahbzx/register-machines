module Tests where
-- Imperial College Haskell Test Suite
import IC.TestSuite hiding (Id)
import qualified IC.TestSuite as TS
import RegisterMachines

--Test cases I wanted to add but kept getting errors when trying to add it
--to the allTestCases, and TAs I asked didn't know what is wrong either
-- showExpTests
--   = [
--       (Val 7) ==> "7.0",
--       (BinApp Add (Id "x") (Id "y")) ==> "x+y",
--       (e1) ==> ("5.0*x"),
--       (e2) ==> ("x*x+y+-(7.0)"),
--       (e4) ==> ("-(cos(x))"),
--       (e5) ==> ("sin(1.0+log(2.0*x))")
--   ]



--Tests for diff function with updated overloaded operator rules for simplification
-- diffExtensionTest
--   = [ (negate (Val 0), "x") ==> Val 0,
--       (e1, "x")             ==> Val 5.0,
--       (e2, "x")             ==> BinApp Add (Id "x") (Id "x"),
--       (Val 1/ Id "x", "x")  ==> BinApp Div (UnApp Neg (Val 1.0)) (BinApp Mul (Id "x") (Id "x")),
--       (Val 0/ e3, "x")      ==> Val 0

--   ]

-- evalTests
--   = [ ((Val 7),  [("x",380)])
--         ==> 7.0

--     , ((Id "a"), [("x",380), ("a",42), ("t",10)])
--         ==> 42.0

--     , ( (BinApp Add (Val (-5)) (Id "t'")), [("t",10), ("t'",18)])
--         ==> 13.0

--     , ((UnApp Neg (BinApp Add (Val (-5)) (Id "t'"))), [("t",10), ("t'",19)])
--         ==> (-14.0)

--     , ((BinApp Mul (Id "x") (Id "x")), [("t",10), ("t'",18.6), ("x",-55)])
--         ==> 3025.0

--     , ((BinApp Div (Val 3) (Id "z")), [("z",7)])
--         ==> 0.42857142857142855

--     , ((UnApp Neg (Id "x")), [("x",0.37)])
--         ==> (-0.37)

--     , ((UnApp Sin (Val 2.4)), [])
--         ==> 0.675463180551151

--     , ((UnApp Cos (Val 2.4)), [])
--         ==> (-0.7373937155412454)

--     , ( e1, [("x",0.37)])
--         ==> (1.85)

--     , ( e2, [("x",0.37), ("y", 8.2)])
--         ==> 1.3369

--     , ( e3, [("x",0.37), ("y", 2.0)])
--         ==> 4.216153846153846

--     , ( e4, [("x",0.37)])
--         ==> (-0.9323273456060345)

--     , ( e5, [("x",0.37)])
--         ==> 0.6433720724587564

--     , ( e6, [("x",0.37)])
--         ==> 0.8799171617597958
--     ]

findInstrPosTests = []

canDecrementTests = []

decrementAtTests = []

incrementAtTests = []

isHaltTests = [
    (program1, (0,[0,1,2])) ==>  False,
    (program1, state1) ==>  True,
    (program2, state2) ==>  True,
    (program3, state3) ==>  True,
    (program3, (5,[0,0])) ==>  True
    ]

executeInstructionTests = []

getNextInstructionTests 
    = [ (program1, (0,[0,1,2])) ==>  i0,
        (program1, (1,[0,1,2])) ==> i1,
        (program1, (2,[0,1,2])) ==> i2,
        (program1, (3,[0,1,2])) ==> i3,
        (program1, (4,[0,1,2])) ==> i4,
        comp1 ==> i0,
        comp2 ==> head program2,
        comp3 ==> head program3
      ]

runTest = [
    comp1 ==> state1,
    comp2 ==> state2,
    comp3 ==> state3
    ]

runShowAllStatesTests = [
    comp1 ==> allStates1,
    comp2 ==> allStates2,
    comp3 ==> allStates3
    ]


-- allTestCases
--   = [ floatTestCase "eval"       (uncurry eval)       evalTests
--     , testCase "diff"            (uncurry diff)       diffTests
--     , floatTestCase "maclaurin"  (uncurry3 maclaurin) maclaurinTests
-- --  , testCase "diffExtension"   (uncurry diff)        diffExtensionTest
--     ]

allTestCases 
  = [ testCase "run" run runTest,
      testCase "runShowAllStates" runShowAllStates runShowAllStatesTests,
      testCase "getNextInstruction" getNextInstruction getNextInstructionTests,
      testCase "isHalt" isHalt isHaltTests
    ]

runTests = mapM_ goTest allTestCases

main = runTests

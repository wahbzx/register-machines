module Tests where
-- Imperial College Haskell Test Suite
import IC.TestSuite hiding (Id)
import qualified IC.TestSuite as TS
import RegisterMachines

findInstrPosTests 
    = [ (L 0, program1) ==>  0,
        (L 2, program2) ==>  2,
        (L 4, program3) ==>  4,
        (L 7, program1) ==>  (-1),
        (L 8, program2) ==> (-1),
        (L 1000, program3) ==> (-1)]

isHaltTests = [
    (program1, (0,[0,1,2])) ==>  False,
    (program1, state1) ==>  True,
    (program2, state2) ==>  True,
    (program3, state3) ==>  True,
    (program3, (5,[0,0])) ==>  True ]

canDecrementTests 
    = [ (snd i0, comp1) ==> True,
        (snd i1, comp1) ==> False,
        (HALT, comp1) ==> False ]

decrementAtTests 
    = [ (0, [1,0,0]) ==> [0,0,0],
        (1, [0,1,0]) ==> [0,0,0],
        (2, [0,0,1]) ==> [0,0,0]]

incrementAtTests
    = [ (0, [0,0,0]) ==> [1,0,0],
        (1, [0,0,0]) ==> [0,1,0],
        (2, [0,0,0]) ==> [0,0,1]]

getNextInstructionTests 
    = [ (program1, (0,[0,1,2])) ==>  i0,
        (program1, (1,[0,1,2])) ==> i1,
        (program1, (2,[0,1,2])) ==> i2,
        (program1, (3,[0,1,2])) ==> i3,
        (program1, (4,[0,1,2])) ==> i4,
        comp1 ==> i0,
        comp2 ==> head program2,
        comp3 ==> head program3 ]

executeInstructionTests 
    = [ (i0, (program1, (0,[0,1,2]))) ==> (1,[0,0,2]),
        (i1, (program1 ,(1,[0,0,2]))) ==> (0,[1,0,2]),
        (i0, (program1 ,(0,[1,0,2]))) ==> (2,[1,0,2]),
        (i2, (program1 ,(2,[1,0,2]))) ==> (3,[1,0,1]),
        (i3, (program1 ,(3,[1,0,1]))) ==> (2,[2,0,1]),
        (i2, (program1 ,(2,[2,0,1]))) ==> (3,[2,0,0]),
        (i3, (program1 ,(3,[2,0,0]))) ==> (2,[3,0,0]),
        (i2, (program1 ,(2,[3,0,0]))) ==> (4,[3,0,0])]

computeTest = [
    comp1 ==> state1,
    comp2 ==> state2,
    comp3 ==> state3 ]

computeShowAllStatesTests 
    = [ comp1 ==> allStates1,
        comp2 ==> allStates2,
        comp3 ==> allStates3 ]

allTestCases 
  = [ testCase "findInstrPos" (uncurry findInstrPos) findInstrPosTests,
      testCase "isHalt" isHalt isHaltTests,
      testCase "canDecrement" (uncurry canDecrement) canDecrementTests,
      testCase "decrementAt" (uncurry decrementAt) decrementAtTests,
      testCase "incrementAt" (uncurry incrementAt) incrementAtTests,
      testCase "executeInstruction" (uncurry executeInstruction) executeInstructionTests,
      testCase "getNextInstruction" getNextInstruction getNextInstructionTests,
      testCase "compute" compute computeTest,
      testCase "computeShowAllStates" computeShowAllStates computeShowAllStatesTests ]

runAllTests = mapM_ goTest allTestCases

main = runAllTests

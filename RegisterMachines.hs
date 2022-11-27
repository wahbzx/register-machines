module RegisterMachines where
{-# LANGUAGE FlexibleContexts #-}

import Data.List
import Data.Maybe

type Name = Int
type Value = Int
type State = (Name, [Value])

newtype Label = L Name deriving (Eq)
data InstructionBody = RPlus Name Label | RMinus Name Label Label | HALT deriving (Eq)
data Result = Res1 Label | Res2 Label Label deriving (Eq)

type Instruction = (Label, InstructionBody)
type Program = [Instruction]
type Computation = (Program, State)

instance Show InstructionBody where
    show HALT = "HALT"
    show (RPlus name l1) = "R" ++ show name ++ "+ -> " ++ show l1
    show (RMinus name l1 l2) = "R" ++ show name ++ "- -> " ++ show l1 ++ "," ++ show l2

instance Show Label where
    show (L name) = "L" ++ show name

instance Show Result where
    show (Res1 label) = show label
    show (Res2 l1 l2) = show l1 ++ "," ++ show l2


findInstrPos :: Label -> Program -> Int
findInstrPos inst ps = res
    where
        res = fromMaybe (-1) (elemIndex inst (map fst ps))

isHalt :: Computation -> Bool
isHalt e@(ps, (l,vs))
    = snd (ps !! findInstrPos (L l) ps) == HALT

canDecrement :: InstructionBody -> Computation -> Bool
canDecrement (RMinus reg t f) (ps, state@(l, vs)) = vs !! reg > 0
canDecrement _ _ = False

decrementAt :: Int -> [Int] -> [Int]
decrementAt n xs 
    | n >= length xs = error "decrementAt: Out of Range"
    | otherwise = a ++ (b - 1) : bs
    where
        (a, b : bs) = splitAt n xs

incrementAt :: Int -> [Int] -> [Int]
incrementAt n xs 
    | n >= length xs = error "incrementAt: Out of Range"
    | otherwise = a ++ (b + 1) : bs
    where
        (a, b : bs) = splitAt n xs

getNextInstruction :: Computation -> Instruction
getNextInstruction (ps, state) = ps !! findInstrPos (L (fst state)) ps

executeInstruction :: Instruction -> Computation -> State
executeInstruction (_ , HALT) (_ , state) = state
executeInstruction (label , i@(RMinus reg (L t) (L f))) exec@(ps , (l, vs)) 
    | canDecrement i exec = (t, decrementAt reg vs)
    | otherwise = (f, vs)
executeInstruction (label , i@(RPlus reg (L n))) exec@(ps , (l, vs)) = (n, incrementAt reg vs)

{-  Returns the final state of the registers 
    after the program has finished executing -} 

compute :: Computation -> State
compute e@(program, state)
    | isHalt (program, res) = res
    | otherwise = compute (program, res)
    where
        res = executeInstruction next e
        next = getNextInstruction e

{-  Final State located at the head of the list  
    and initial state is the last element of the list -} 

computeShowAllStates :: Computation -> [State]
computeShowAllStates e@(program, state)
    = computeShowAll' e [state]
    where
        computeShowAll' e@(program, state) ls
            | isHalt (program, res) = res:ls
            | otherwise = computeShowAll' (program, res) (res:ls)
            where
                res = executeInstruction next e
                next = getNextInstruction e

-- Examples

comp1 :: Computation
comp1 = (program1, (0,[0,1,2]))

state1 :: State
state1 = (4,[3,0,0])

allStates1 :: [State]
allStates1 = [(4,[3,0,0]),(2,[3,0,0]),(3,[2,0,0]),
              (2,[2,0,1]),(3,[1,0,1]),(2,[1,0,2]),
              (0,[1,0,2]),(1,[0,0,2]),(0,[0,1,2])]

comp2 :: Computation
comp2 = (program2, (0,[0,2,3,0]))

state2 :: State
state2 = (6,[6,0,3,0])

allStates2 :: [State]
allStates2 = [(6,[6,0,3,0]),(0,[6,0,3,0]),(4,[6,0,3,0]),
              (5,[6,0,2,0]),(4,[6,0,2,1]),(5,[6,0,1,1]),
              (4,[6,0,1,2]),(5,[6,0,0,2]),(4,[6,0,0,3]),
              (1,[6,0,0,3]),(3,[6,0,0,2]),(2,[5,0,0,2]),
              (1,[5,0,1,2]),(3,[5,0,1,1]),(2,[4,0,1,1]),
              (1,[4,0,2,1]),(3,[4,0,2,0]),(2,[3,0,2,0]),
              (1,[3,0,3,0]),(0,[3,1,3,0]),(4,[3,1,3,0]),
              (5,[3,1,2,0]),(4,[3,1,2,1]),(5,[3,1,1,1]),
              (4,[3,1,1,2]),(5,[3,1,0,2]),(4,[3,1,0,3]),
              (1,[3,1,0,3]),(3,[3,1,0,2]),(2,[2,1,0,2]),
              (1,[2,1,1,2]),(3,[2,1,1,1]),(2,[1,1,1,1]),
              (1,[1,1,2,1]),(3,[1,1,2,0]),(2,[0,1,2,0]),
              (1,[0,1,3,0]),(0,[0,2,3,0])]

comp3 :: Computation
comp3 = (program3, (0,[0,7]))

state3 :: State
state3 = (2,[2,0])

allStates3 :: [State]
allStates3 = [(2,[2,0]),(3,[2,0]),(0,[2,1]),(4,[1,1]),(1,[1,2]),(3,[1,3]),(0,[1,4]),(4,[0,4]),(1,[0,5]),(3,[0,6]),(0,[0,7])]


i0 :: Instruction
i0 = (L 0, RMinus 1 (L 1) (L 2))
i1 :: Instruction
i1 = (L 1, RPlus 0 (L 0))
i2 :: Instruction
i2 = (L 2, RMinus 2 (L 3) (L 4))
i3 :: Instruction
i3 = (L 3, RPlus 0 (L 2))
i4 :: Instruction
i4 = (L 4, HALT)

program1 :: [(Label, InstructionBody)]
program1 = [i0, i1, i2, i3 , i4]

program2 :: Program
program2 = [(L 0, RMinus 1 (L 1) (L 6)),
            (L 1, RMinus 2 (L 2) (L 4)),
            (L 2, RPlus 0  (L 3)),
            (L 3, RPlus 3 (L 1)),
            (L 4, RMinus 3 (L 5) (L 0)),
            (L 5, RPlus 2 (L 4)),
            (L 6, HALT)]

program3 :: Program
program3 = [(L 0, RMinus 1 (L 3) (L 5)),
            (L 1, RMinus 1 (L 4) (L 2)),
            (L 2, HALT),
            (L 3, RMinus 1 (L 1) (L 2)),
            (L 4, RPlus 0 (L 0)),
            (L 5, HALT)]
            
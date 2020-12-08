{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens (makeLenses, over, view)
import Control.Monad (unless)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List.Extra (firstJust)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (__i)
import Data.Void (Void)
import Polysemy (Member, Sem, run)
import Polysemy.State (State, execState, gets, modify)
import Text.Megaparsec (Parsec, parseMaybe, sepEndBy)
import Text.Megaparsec.Char (newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

type Parser = Parsec Void String

data Operand
  = Acc
  | Nop
  | Jmp
  deriving (Show)

parseOperand :: Parser Operand
parseOperand =
  asum
    [ string "acc" $> Acc,
      string "nop" $> Nop,
      string "jmp" $> Jmp
    ]

type Instruction = (Operand, Int)

parseInstruction :: Parser Instruction
parseInstruction = (,) <$> parseOperand <* space <*> signed space decimal

type Program = [Instruction]

parseProgram :: Parser Program
parseProgram = parseInstruction `sepEndBy` newline

type History = Int -> Bool

data InterpreterState = InterpreterState
  { _accumulator :: Int,
    _instructionHistory :: History,
    _program :: Program, -- we don't actually modify this one
    _programCounter :: Int
  }

makeLenses ''InterpreterState

initialInterpreterState :: Program -> InterpreterState
initialInterpreterState _program =
  InterpreterState
    { _accumulator = 0,
      _instructionHistory = const False,
      _program,
      _programCounter = 0
    }

-- * Building the interpreter

-- | Increments the program counter.
bumpProgramCounter ::
  Member (State InterpreterState) e =>
  Sem e ()
bumpProgramCounter = modify (over programCounter (+ 1))

-- | Performs the wanted operation, and moves the program counter accordingly.
performInstruction ::
  Member (State InterpreterState) e =>
  Instruction ->
  Sem e ()
performInstruction (Acc, arg) = modify (over accumulator (+ arg)) >> bumpProgramCounter
performInstruction (Jmp, arg) = modify (over programCounter (+ arg))
performInstruction (Nop, _) = bumpProgramCounter

-- | Fetches an instruction, will fail if the instruction is out of bounds.
fetchInstruction ::
  Member (State InterpreterState) e =>
  Int ->
  Sem e Instruction
fetchInstruction pc =
  (!!) <$> gets (view program) <*> pure pc

-- | Updates a history to mark a program counter as seen.
seen :: Int -> History -> History
seen seenProgramCounter oldHistory target =
  target == seenProgramCounter || oldHistory target

-- | Adds the given program counter to the instruction history.
markInstruction ::
  Member (State InterpreterState) e =>
  Int ->
  Sem e ()
markInstruction pc = modify (over instructionHistory (seen pc))

-- | Runs the program until the program counter ends up on a previously-executed
-- instruction, or exceeds the last instruction of the program.
runProgram ::
  Member (State InterpreterState) e =>
  Sem e ()
runProgram =
  do
    p <- gets (view program)
    pc <- gets (view programCounter)
    wasAlreadyVisited <- gets (view instructionHistory)
    unless (wasAlreadyVisited pc || pc >= length p) $ do
      markInstruction pc
      performInstruction =<< fetchInstruction pc
      runProgram

-- | Runs a program to completion/loop and returns its final state.
programFinalState :: Program -> InterpreterState
programFinalState myProgram =
  run (execState (initialInterpreterState myProgram) runProgram)

-- * Part 1

-- | Runs a program until it loops and returns the value of the accumulator as
-- the looping begins.
valueInAccumulatorBeforeLooping :: Program -> Int
valueInAccumulatorBeforeLooping =
  view accumulator . programFinalState

-- * Part 2

-- | Computes all the alternatives of a program where exactly one of the Jmp/Nop
-- instructions was switched into Nop/Jmp respectively.
programAlternatives :: Program -> [Program]
programAlternatives [] = [[]]
programAlternatives (instr@(Acc, _) : instrs) =
  (instr :) <$> programAlternatives instrs
programAlternatives (instr@(Jmp, arg) : instrs) =
  ((Nop, arg) : instrs) : ((instr :) <$> programAlternatives instrs)
programAlternatives (instr@(Nop, arg) : instrs) =
  ((Jmp, arg) : instrs) : ((instr :) <$> programAlternatives instrs)

-- | Checks whether a program runs to completion without looping, and if so,
-- returns its final accumulator.
checkProgramAndGrabFinalAccumulator :: Program -> Maybe Int
checkProgramAndGrabFinalAccumulator myProgram =
  let finalState = programFinalState myProgram
   in if view programCounter finalState == length myProgram
        then Just (view accumulator finalState)
        else Nothing

-- | Finds the one alternative of the input program that terminates without
-- looping, and returns its final accumulator.
accumulatorForAlternativeEndingWithoutLooping :: Program -> Maybe Int
accumulatorForAlternativeEndingWithoutLooping myProgram =
  firstJust checkProgramAndGrabFinalAccumulator (programAlternatives myProgram)

-- * Executable

main :: IO ()
main =
  do
    contents <- readFile "day08/input/real.txt"
    let myProgram = fromMaybe [] (parseMaybe parseProgram contents)
    let solution1 = valueInAccumulatorBeforeLooping myProgram
    let solution2 = fromMaybe 0 (accumulatorForAlternativeEndingWithoutLooping myProgram)
    putStrLn [__i|Problem 1: #{solution1}|]
    putStrLn [__i|Problem 2: #{solution2}|]

-- * Tests

-- >>> parseMaybe parseInstruction "acc -99"
-- Just (Acc,-99)

-- >>> parseMaybe parseProgram "acc -99\njmp 3\nnop 42\n"
-- Just [(Acc,-99),(Jmp,3),(Nop,42)]

historyBefore :: History
historyBefore pc = pc == 2 || pc == 4

historyAfter :: History
historyAfter = seen 7 historyBefore

-- >>> (historyBefore <$> [0..10])
-- [False,False,True,False,True,False,False,False,False,False,False]

-- >>> (historyAfter <$> [0..10])
-- [False,False,True,False,True,False,False,True,False,False,False]

program1 :: Program
program1 = [(Acc, 3), (Jmp, -1), (Nop, 42)]

-- >>> programAlternatives program1
-- [[(Acc,3),(Nop,-1),(Nop,42)],[(Acc,3),(Jmp,-1),(Jmp,42)],[(Acc,3),(Jmp,-1),(Nop,42)]]

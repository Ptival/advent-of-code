{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow ((>>>))
import Control.Lens (makeLenses, over, set, view)
import Control.Monad (forM_)
import Data.Bits (Bits (clearBit, setBit))
import Data.Finite (Finite, getFinite)
import Data.Foldable (asum)
import Data.Functor (($>))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.String.Interpolate (i, __i)
import Data.TypeNums (Nat, natVal)
import qualified Data.Vector.Sized as V
import Data.Void (Void)
import Polysemy (Member, Sem, run)
import Polysemy.State (State, execState, gets, modify)
import Text.Megaparsec (Parsec, parseMaybe, sepEndBy, some, try)
import Text.Megaparsec.Char (char, digitChar, newline, string)
import Prelude hiding (Either (..))

-- * Type definitions

type Parser = Parsec Void String

data BitMaskBit
  = MaskZero
  | MaskOne
  | MaskFloat

instance Show BitMaskBit where
  show MaskZero = "0"
  show MaskOne = "1"
  show MaskFloat = "X"

type BitSize = 36 :: Nat

bitSize :: Integer
bitSize = natVal (Proxy :: Proxy BitSize)

type BitMask = V.Vector BitSize BitMaskBit

data Instruction
  = SetMask BitMask
  | Write Integer Integer

instance Show Instruction where
  show (SetMask m) = [i|mask #{m}|]
  show (Write m v) = [i|m[#{m}] <- #{v}|]

type Program = [Instruction]

newtype DecoderVersion = DecoderVersion {getDecoderVersion :: Int}

data ProgramState = ProgramState
  { _currentMask :: BitMask,
    _decoderChipVersion :: DecoderVersion,
    _memory :: M.Map Integer Integer
  }

makeLenses ''ProgramState

-- * Parsing

integer :: Parser Integer
integer = read <$> some digitChar

bitMaskBit :: Parser BitMaskBit
bitMaskBit = asum [char '0' $> MaskZero, char '1' $> MaskOne, char 'X' $> MaskFloat]

-- | We 'reverse' here so that the bit at index 0 is the bit of order 0.
bitMask :: Parser BitMask
bitMask = V.reverse <$> V.generateM (const bitMaskBit)

setMask :: Parser Instruction
setMask = SetMask <$> (string "mask = " *> bitMask)

write :: Parser Instruction
write = Write <$> (string "mem[" *> integer <* string "] = ") <*> integer

instruction :: Parser Instruction
instruction = asum [try setMask, write]

program :: Parser Program
program = instruction `sepEndBy` newline

-- * General utilities

-- | Get the value of the finite number as an 'Int'.
finiteInt :: Finite BitSize -> Int
finiteInt = fromIntegral . getFinite

-- * Problem utilities

type IntegerBitMask = Integer -> Integer

performInstruction :: Member (State ProgramState) e => Instruction -> Sem e ()
performInstruction (SetMask mask) = modify (set currentMask mask)
performInstruction (Write address value) =
  do
    mask <- gets (view currentMask)
    gets (view decoderChipVersion >>> getDecoderVersion) >>= \case
      1 -> writeV1 address mask value
      2 -> writeV2 address mask value
      v -> error [i|Unsupported decoded chip version #{v}.|]

runProgram :: Member (State ProgramState) e => Program -> Sem e ()
runProgram = sequence_ . (performInstruction <$>)

initialProgramState :: DecoderVersion -> ProgramState
initialProgramState version =
  ProgramState
    { _currentMask = V.replicate MaskFloat,
      _decoderChipVersion = version,
      _memory = M.empty
    }

-- | Returns the sum of all assigned addresses after a run of the given program.
sumMemoryAfterExecution :: DecoderVersion -> Program -> Integer
sumMemoryAfterExecution version myProgram =
  let finalState = run (execState (initialProgramState version) (runProgram myProgram))
   in sum (M.elems (view memory finalState))

-- * Part 1

-- | V1 semantics: 0 means clear bit, 1 means set bit, X means preserve bit.
maskV1 :: BitMask -> IntegerBitMask
maskV1 = V.ifoldr' maskThatBit id
  where
    maskThatBit :: Finite BitSize -> BitMaskBit -> IntegerBitMask -> IntegerBitMask
    maskThatBit order MaskZero = (.) (`clearBit` finiteInt order)
    maskThatBit order MaskOne = (.) (`setBit` finiteInt order)
    maskThatBit _ MaskFloat = id

-- | Version 1 of the write operation, masks the value and writes it at the
-- given address.
writeV1 :: Member (State ProgramState) e => Integer -> BitMask -> Integer -> Sem e ()
writeV1 address mask value =
  modify (over memory (M.insert address (maskV1 mask value)))

partOne :: Program -> Integer
partOne = sumMemoryAfterExecution (DecoderVersion 1)

-- * Part 2

-- | V2 semantics: 0 means preserve bit, 1 means set bit, X means consider both
-- bit values.
maskV2 :: BitMask -> [IntegerBitMask]
maskV2 = V.ifoldr' maskThatBit [id]
  where
    maskThatBit :: Finite BitSize -> BitMaskBit -> [IntegerBitMask] -> [IntegerBitMask]
    maskThatBit _ MaskZero = id
    maskThatBit order MaskOne = ((.) (`setBit` finiteInt order) <$>)
    maskThatBit order MaskFloat = setOrClear
      where
        setOrClear :: [IntegerBitMask] -> [IntegerBitMask]
        setOrClear masks =
          [ (`op` finiteInt order) . mask
            | mask <- masks,
              op <- [clearBit, setBit]
          ]

-- | Version 2 of the write operation, computes the set of addresses from the
-- mask and writes the original value at all such addresses.
writeV2 :: Member (State ProgramState) e => Integer -> BitMask -> Integer -> Sem e ()
writeV2 address originalMask value =
  forM_ (maskV2 originalMask) $ \mask ->
    modify (over memory (M.insert (mask address) value))

partTwo :: Program -> Integer
partTwo = sumMemoryAfterExecution (DecoderVersion 2)

-- * Executable

main :: IO ()
main =
  do
    contents <- readFile "day14/input/real.txt"
    let myProgram = fromMaybe [] (parseMaybe program contents)
    let solution1 = partOne myProgram
    let solution2 = partTwo myProgram
    putStrLn [__i|Problem 1: #{solution1}|]
    putStrLn [__i|Problem 2: #{solution2}|]

-- * Tests

testString :: String
testString =
  [i|mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
|]

testInput :: Program
testInput = fromMaybe [] (parseMaybe program testString)

secondTestString :: String
secondTestString =
  [i|mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
|]

secondTestInput :: Program
secondTestInput = fromMaybe [] (parseMaybe program secondTestString)

-- >>> maskV1 (V.replicate MaskZero) 42 == 0
-- True

-- >>> maskV1 (V.replicate MaskOne) 42 == 2 ^ bitSize - 1
-- True

-- >>> maskV1 (V.replicate MaskFloat) 42 == 42
-- True

-- >>> partOne testInput
-- 165

-- >>> partTwo secondTestInput
-- 208

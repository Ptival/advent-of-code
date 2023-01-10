{-# LANGUAGE QuasiQuotes #-}

module Main where

import AdventOfCode (runDay)
import Control.Applicative (asum)
import Control.Lens (Lens', set, view, (%~))
import Control.Monad (forM_, replicateM_, when)
import Control.Monad.Freer (Eff, Member, Members)
import Control.Monad.Freer.Reader (Reader, ask, runReader)
import Control.Monad.Freer.State (State, evalState, get, gets, modify)
import Control.Monad.Freer.Trace (Trace, runTrace, trace)
import Data.Bool (bool)
import Data.Finite (Finite, getFinite, packFinite)
import Data.Generics.Product (HasField (field))
import Data.String.Interpolate (i)
import qualified Data.Vector.Sized as V
import GHC.Data.Maybe (whenIsJust)
import GHC.Generics (Generic)
import MegaparsecExtras (Parser, parseNumber, parseOrFail)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (newline, space, string)

data Instr
  = Addx Int
  | Noop
  deriving (Show)

parseInstr :: Parser Instr
parseInstr =
  asum
    [ pure Noop <* string "noop",
      Addx <$> (string "addx" *> space *> parseNumber)
    ]

newtype Cycle = Cycle {_getCycle :: Int} deriving (Enum, Eq, Generic, Integral, Num, Ord, Real, Show)

getCycle :: Lens' Cycle Int
getCycle = field @"_getCycle"

newtype Registers = Registers {_xRegister :: Int} deriving (Generic, Show)

xRegister :: Lens' Registers Int
xRegister = field @"_xRegister"

newtype SignalStrength = SignalStrength {_signalStrength :: Int} deriving (Generic, Show)

signalStrength :: Lens' SignalStrength Int
signalStrength = field @"_signalStrength"

type PreCycleHandlerEffs =
  '[State CRT, State Cycle, State Registers, State SignalStrength, Trace]

newtype PreCycleHandler subset = PreCycleHandler
  {getPreCycleHandler :: forall effs. Members subset effs => Eff effs ()}

type InstrEffs effs =
  ( Members PreCycleHandlerEffs effs,
    Member (Reader (PreCycleHandler PreCycleHandlerEffs)) effs
  )

runPreCycleHandler :: InstrEffs effs => Eff effs ()
runPreCycleHandler = do
  hdlr <- ask
  getPreCycleHandler @PreCycleHandlerEffs hdlr

consumeCycle :: InstrEffs effs => Eff effs ()
consumeCycle = do
  runPreCycleHandler
  -- trace [i|Consuming cycle|]
  modify $ getCycle %~ (+ 1)

noop :: InstrEffs effs => Eff effs ()
noop = do
  -- trace "Executing no-op"
  consumeCycle

addx ::
  InstrEffs effs =>
  Int ->
  Eff effs ()
addx n = do
  -- trace [i|Executing (addx #{n})|]
  replicateM_ 2 consumeCycle
  modify $ xRegister %~ (+ n)

dispatchInstr ::
  InstrEffs effs =>
  Instr ->
  Eff effs ()
dispatchInstr Noop = noop
dispatchInstr (Addx n) = addx n

parse :: String -> [Instr]
parse = parseOrFail $ parseInstr `sepEndBy` newline

preCycleHandlerForPart1 ::
  Members PreCycleHandlerEffs effs =>
  Eff effs ()
preCycleHandlerForPart1 = do
  c <- view getCycle <$> get @Cycle
  when (c `elem` [20, 60 .. 220]) $ do
    rx <- view xRegister <$> get @Registers
    modify $ signalStrength %~ (+ (c * rx))

evalSignalStrength :: Eff (State SignalStrength : effs) a -> Eff effs a
evalSignalStrength = evalState (SignalStrength 0)

evalRegisters :: Eff (State Registers : effs) a -> Eff effs a
evalRegisters = evalState (Registers 1)

evalCycle :: Eff (State Cycle : effs) a -> Eff effs a
evalCycle = evalState (Cycle 1)

solvePart1 :: String -> IO String
solvePart1 s = do
  let instrs = parse s
  result <- runTrace
    . evalSignalStrength
    . evalRegisters
    . evalCycle
    . evalCRT
    . runReader (PreCycleHandler @PreCycleHandlerEffs preCycleHandlerForPart1)
    $ do
      forM_ instrs dispatchInstr
      view signalStrength <$> get
  return $ show result

-- * Part 2

type CRTWidth = 40

type CRTHeight = 6

type CRTPosition = (Finite CRTHeight, Finite CRTWidth)

data CRT = CRT
  { _pixels :: V.Vector CRTHeight (V.Vector CRTWidth Bool),
    _headPosition :: CRTPosition
  }
  deriving (Generic, Show)

pixels :: Lens' CRT (V.Vector CRTHeight (V.Vector CRTWidth Bool))
pixels = field @"_pixels"

headPosition :: Lens' CRT CRTPosition
headPosition = field @"_headPosition"

nextCRTPosition :: CRTPosition -> Maybe CRTPosition
nextCRTPosition (row, col) =
  case packFinite (getFinite col + 1) of
    Nothing -> (,0) <$> packFinite (getFinite row + 1)
    Just col' -> Just (row, col')

type CRTEffs effs =
  ( Members PreCycleHandlerEffs effs,
    Member (Reader (PreCycleHandler PreCycleHandlerEffs)) effs
  )

newtype PreCycleHandlerCRT = PreCycleHandlerCRT
  {getPreCycleHandlerCRT :: forall effs. CRTEffs effs => Eff effs ()}

runPreCycleHandlerCRT :: CRTEffs effs => Eff effs ()
runPreCycleHandlerCRT = do
  hdlr <- ask
  getPreCycleHandler @PreCycleHandlerEffs hdlr

type PreCycleHandlerCRTEffs =
  State CRT ': PreCycleHandlerEffs

drawPixel ::
  Members PreCycleHandlerCRTEffs effs =>
  Eff effs ()
drawPixel = do
  pos@(row, col) <- gets $ view headPosition
  rx <- gets $ view xRegister
  let pixelValue = abs (rx - fromInteger (getFinite col)) <= 1
  trace [i|Pixel value is #{pixelValue} because rx is #{rx} and col is #{col}|]
  modify $ set (pixels . V.ix row . V.ix col) pixelValue
  whenIsJust (nextCRTPosition pos) $ modify . set headPosition
  pos' <- gets $ view headPosition
  trace $ "New position: " <> show pos'

printScreen ::
  Members PreCycleHandlerCRTEffs effs =>
  Eff effs ()
printScreen = do
  screen <- gets $ view pixels
  forM_ (V.toList screen) $ \row ->
    trace $ map (bool '.' '#') $ V.toList row
  trace ""

runInstrOnCRT ::
  CRTEffs effs =>
  Instr ->
  Eff effs ()
runInstrOnCRT instr = do
  dispatchInstr instr
  printScreen

preCycleHandlerCRT :: Members PreCycleHandlerCRTEffs effs => Eff effs ()
preCycleHandlerCRT = do
  drawPixel

evalCRT :: Eff (State CRT : effs) a -> Eff effs a
evalCRT = evalState (CRT (V.replicate (V.replicate False)) (0, 0))

solvePart2 :: String -> IO String
solvePart2 s = do
  let instrs = parse s
  () <- runTrace
    . evalSignalStrength
    . evalRegisters
    . evalCycle
    . evalCRT
    . runReader (PreCycleHandler @PreCycleHandlerEffs preCycleHandlerCRT)
    $ forM_ instrs runInstrOnCRT
  return "Must read the text in the output above"

main :: IO ()
main = runDay 10 solvePart1 solvePart2

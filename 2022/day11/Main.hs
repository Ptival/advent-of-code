{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import AdventOfCode (runDay)
import Control.Applicative (asum)
import Control.Arrow ((>>>))
import Control.Lens (Ixed (ix), Lens', over, view, _Just)
import Control.Lens.At (At (at))
import Control.Lens.Tuple (_1)
import Control.Monad (forM_, when)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Reader (Reader, ask, asks, runReader)
import Control.Monad.Freer.State (State, evalState, gets, modify)
import Control.Monad.Freer.Trace (Trace, runTrace, trace)
import Data.Generics.Product (field)
import Data.List (sortBy)
import Data.List.Index (ireplicateM_)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import Data.String.Interpolate (i)
import FreerExtras (modifyingReturningOld)
import FunctionExtras (($>>>))
import GHC.Generics (Generic)
import MegaparsecExtras (Parser, parseNumber, parseOrFail)
import Text.Megaparsec (sepBy, sepEndBy)
import Text.Megaparsec.Char (char, newline, space, string)
import Prelude hiding (round)

traceEverything :: Bool
traceEverything = False

newtype MonkeyID = MonkeyID {getMonkeyID :: Int} deriving (Eq, Generic, Ord, Show)

data Term
  = Old
  | Num Integer
  deriving (Generic, Show)

data Operator
  = Mul
  | Plus
  deriving (Generic, Show)

type Operation = (Term, Operator, Term)

data Monkey = Monkey
  { _monkeyID :: MonkeyID,
    _items :: [Integer],
    _operation :: Operation,
    _test :: (Integer, MonkeyID, MonkeyID),
    _inspectionCount :: Integer
  }
  deriving (Generic, Show)

monkeyID :: Lens' Monkey MonkeyID
monkeyID = field @"_monkeyID"

items :: Lens' Monkey [Integer]
items = field @"_items"

operation :: Lens' Monkey Operation
operation = field @"_operation"

test :: Lens' Monkey (Integer, MonkeyID, MonkeyID)
test = field @"_test"

inspectionCount :: Lens' Monkey Integer
inspectionCount = field @"_inspectionCount"

parseOperator :: Parser Operator
parseOperator = asum [char '+' *> pure Plus, char '*' *> pure Mul]

parseTerm :: Parser Term
parseTerm =
  asum
    [ string "old" *> pure Old,
      Num <$> parseNumber
    ]

parseOperation :: Parser Operation
parseOperation = (,,) <$> parseTerm <*> (space *> parseOperator <* space) <*> parseTerm

parseMonkeyID :: Parser MonkeyID
parseMonkeyID = MonkeyID <$> parseNumber

parseMonkey :: Parser (MonkeyID, Monkey)
parseMonkey = do
  _monkeyID <- string "Monkey " *> parseMonkeyID <* char ':' <* newline
  _items <- string "  Starting items: " *> (parseNumber `sepBy` string ", ") <* newline
  _operation <- string "  Operation: new = " *> parseOperation <* newline
  condition <- string "  Test: divisible by " *> parseNumber <* newline
  trueBranch <- string "    If true: throw to monkey " *> parseMonkeyID <* newline
  falseBranch <- string "    If false: throw to monkey " *> parseMonkeyID <* newline
  return
    ( _monkeyID,
      Monkey
        { _monkeyID,
          _items,
          _operation,
          _test = (condition, trueBranch, falseBranch),
          _inspectionCount = 0
        }
    )

newtype Monkeys = Monkeys {_monkeys :: Map.Map MonkeyID Monkey}
  deriving (Generic, Show)

monkeys :: Lens' Monkeys (Map.Map MonkeyID Monkey)
monkeys = field @"_monkeys"

data DivideWorryAfterInspection
  = DoDivideWorryAfterInspection
  | DontDivideWorryAfterInspection
  deriving (Eq)

turn ::
  Member (Reader CommonDivisor) effs =>
  Member (Reader DivideWorryAfterInspection) effs =>
  Member (State Monkeys) effs =>
  Member Trace effs =>
  MonkeyID ->
  Eff effs ()
turn turnMonkeyID = do
  when traceEverything $
    trace [i|Monkey #{getMonkeyID turnMonkeyID}:|]
  turnMonkey <- fromJust <$> (gets $ view $ monkeys . at turnMonkeyID)
  turnMonkeyItems <- modifyingReturningOld (monkeys . at turnMonkeyID . _Just . items) (const [])
  forM_ turnMonkeyItems $ \(item :: Integer) -> do
    when traceEverything $
      trace [i|  Monkey inspects an item with a worry level of #{item}.|]
    modify $ over (monkeys . at turnMonkeyID . _Just . inspectionCount) (+ 1)
    let (_t1, op, t2) = view operation turnMonkey
    let (thisOperation, describeOperation) =
          case op of
            Plus -> ((+), "increases")
            Mul -> ((*), "is multiplied")
    let (operand, describeOperand) =
          case t2 of
            Old -> (item, "itself")
            Num n -> (n, show n)
    let worryLevel1 = item `thisOperation` operand
    when traceEverything $
      trace [i|    Worry level #{describeOperation} by #{describeOperand} to #{worryLevel1}.|]
    divideWorryAfterInspection <- ask
    let worryLevel2 =
          if divideWorryAfterInspection == DoDivideWorryAfterInspection
            then worryLevel1 `div` 3
            else worryLevel1
    when (traceEverything && divideWorryAfterInspection == DoDivideWorryAfterInspection) $
      trace [i|    Monkey gets bored with item. Worry level is divided by 3 to #{worryLevel2}|]
    commonDivisor <- asks getCommonDivisor
    let finalWorryLevel = worryLevel2 `mod` commonDivisor
    let (divisor, onTrue, onFalse) = view test turnMonkey
    let divisible = (finalWorryLevel `mod` divisor) == 0
    let divisibleText = if divisible then "is" else "is not"
    when traceEverything $
      trace [i|    Current worry level #{divisibleText} divisible by #{divisor}.|]
    let receivingMonkey = if divisible then onTrue else onFalse
    when traceEverything $
      trace [i|    Item with worry level #{finalWorryLevel} is sent to monkey #{getMonkeyID receivingMonkey}.|]
    modify @Monkeys $ over (monkeys . ix receivingMonkey . items) $ (<> [finalWorryLevel])

newtype CommonDivisor = CommonDivisor {getCommonDivisor :: Integer}

round ::
  Member (Reader CommonDivisor) effs =>
  Member (Reader DivideWorryAfterInspection) effs =>
  Member (State Monkeys) effs =>
  Member Trace effs =>
  Int ->
  Eff effs ()
round roundNumber = do
  when traceEverything $ trace [i|Round #{roundNumber + 1}|]
  monkeyIDs <- gets $ view monkeys
  forM_ (Map.keys monkeyIDs) turn
  divideWorryAfterInspection <- ask
  when (traceEverything && divideWorryAfterInspection == DontDivideWorryAfterInspection) $
    postRoundInspectionCounts (roundNumber + 1)

postRoundInspectionCounts ::
  Member (Reader DivideWorryAfterInspection) effs =>
  Member (State Monkeys) effs =>
  Member Trace effs =>
  Int ->
  Eff effs ()
postRoundInspectionCounts roundNumber = do
  trace [i|== After round #{roundNumber} ==|]
  myMonkeys <- gets $ view monkeys
  forM_ (Map.elems myMonkeys) $ \monkey -> do
    let thisMonkeyID = getMonkeyID $ view monkeyID monkey
    let number = view inspectionCount monkey
    trace [i|Monkey #{thisMonkeyID} inspected #{number} items.|]

parse :: String -> Monkeys
parse = parseOrFail $ do
  monkeyList <- parseMonkey `sepEndBy` newline
  return $ Monkeys $ Map.fromList monkeyList

computeCommonDivisor :: Monkeys -> Integer
computeCommonDivisor =
  view monkeys
    >>> Map.elems
    >>> map (view (test . _1))
    >>> product

solvePart1 :: String -> IO Integer
solvePart1 s = do
  let myMonkeys = parse s
  runTrace
    . evalState myMonkeys
    . runReader DoDivideWorryAfterInspection
    . runReader (CommonDivisor (computeCommonDivisor myMonkeys))
    $ do
      ireplicateM_ 20 round
      finalMonkeys <- gets $ view monkeys
      forM_ finalMonkeys (trace . show)
      let [(_, monkey1), (_, monkey2)] =
            Map.toList finalMonkeys
              $>>> sortBy (comparing (Down . view inspectionCount . snd))
              >>> take 2
      return $ view inspectionCount monkey1 * view inspectionCount monkey2

solvePart2 :: String -> IO Integer
solvePart2 s = do
  let myMonkeys = parse s
  runTrace
    . evalState myMonkeys
    . runReader DontDivideWorryAfterInspection
    . runReader (CommonDivisor (computeCommonDivisor myMonkeys))
    $ do
      -- ireplicateM_ 10_000 round
      ireplicateM_ 10_000 round
      finalMonkeys <- gets $ view monkeys
      forM_ finalMonkeys (trace . show)
      let [(_, monkey1), (_, monkey2)] =
            Map.toList finalMonkeys
              $>>> sortBy (comparing (Down . view inspectionCount . snd))
              >>> take 2
      return $ view inspectionCount monkey1 * view inspectionCount monkey2

main :: IO ()
main = runDay 11 solvePart1 solvePart2

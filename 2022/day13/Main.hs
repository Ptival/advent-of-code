{-# LANGUAGE QuasiQuotes #-}

module Main where

import AdventOfCode (runDay)
import Control.Applicative (asum)
import Control.Arrow ((>>>))
import Control.Lens (Field1 (_1), over)
import Control.Monad (forM, when)
import Control.Monad.Freer (Eff, Members)
import Control.Monad.Freer.Trace (Trace, runTrace, trace)
import Control.Monad.ListM (sortByM)
import Data.List.Index (indexed)
import Data.String.Interpolate (i)
import Data.Tree (Tree (Node), drawTree)
import FunctionExtras (($>>>))
import MegaparsecExtras (Parser, parseNumber, parseOrFail)
import Text.Megaparsec (sepBy, sepEndBy)
import Text.Megaparsec.Char (char, newline)

type Packet = Tree (Maybe Int)

integerPacket :: Int -> Packet
integerPacket n = Node (Just n) []

listPacket :: [Packet] -> Packet
listPacket = Node Nothing

parsePacket :: Parser Packet
parsePacket =
  asum
    [ integerPacket <$> parseNumber,
      listPacket <$> (char '[' *> (parsePacket `sepBy` char ',') <* char ']')
    ]

parsePacketPair :: Parser (Packet, Packet)
parsePacketPair = do
  p1 <- parsePacket <* newline
  p2 <- parsePacket <* newline
  return (p1, p2)

parse :: String -> [(Packet, Packet)]
parse = parseOrFail (parsePacketPair `sepEndBy` newline)

drawPacketPair :: (Packet, Packet) -> String
drawPacketPair (p1, p2) =
  unlines $ map (drawTree . fmap show) $ [p1, p2]

asInteger :: Packet -> Maybe Int
asInteger (Node (Just n) _) = Just n
asInteger _ = Nothing

asList :: Packet -> Maybe [Packet]
asList (Node Nothing p) = Just p
asList _ = Nothing

myCompare ::
  Members '[Trace] effs =>
  Ord a =>
  Show a =>
  a ->
  a ->
  Eff effs Ordering
myCompare a b = do
  when False $ trace [i|Comparing #{show a} and #{show b}|]
  return $ compare a b

orderedPackets ::
  Members '[Trace] effs =>
  Packet ->
  Packet ->
  Eff effs Ordering
-- Both integers
orderedPackets (asInteger -> Just l) (asInteger -> Just r) = myCompare l r
-- Left integer, right list
orderedPackets l@(asInteger -> Just _) r@(asList -> Just _) =
  orderedPackets (listPacket [l]) r
-- Right integer, left list
orderedPackets l@(asList -> Just _) r@(asInteger -> Just _) =
  orderedPackets l (listPacket [r])
-- Both empty, no decision made yet
orderedPackets (asList -> Just []) (asList -> Just []) = return EQ
-- Left ran out before right
orderedPackets (asList -> Just []) (asList -> Just _) = return LT
-- Right ran out before left
orderedPackets (asList -> Just _) (asList -> Just []) = return GT
-- Both non-empty lists
orderedPackets (asList -> Just (l : ls)) (asList -> Just (r : rs)) =
  orderedPackets l r >>= \case
    LT -> return LT
    EQ -> orderedPackets (listPacket ls) (listPacket rs)
    GT -> return GT
orderedPackets _ _ = error "Should be unreachable"

solvePart1 :: String -> IO String
solvePart1 s = do
  let input = parse s
  results <- runTrace $ forM input $ uncurry orderedPackets
  return $
    results
      $>>> indexed
      >>> map (over _1 (+ 1)) -- indexed is 0-based, we want 1-based
      >>> filter ((/= GT) . snd)
      >>> map fst
      >>> sum
      >>> show

solvePart2 :: String -> IO String
solvePart2 s = do
  let input = parse s
  let dividerPackets =
        [ listPacket [listPacket [integerPacket 2]],
          listPacket [listPacket [integerPacket 6]]
        ]
  results <-
    runTrace $
      sortByM orderedPackets $
        dividerPackets
          <> concatMap (\(p1, p2) -> [p1, p2]) input
  return $
    results
      $>>> indexed
      >>> map (over _1 (+ 1))
      >>> filter ((`elem` dividerPackets) . snd)
      >>> map fst
      >>> product
      >>> show

main :: IO ()
main = runDay 13 solvePart1 solvePart2

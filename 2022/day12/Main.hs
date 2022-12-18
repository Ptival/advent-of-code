module Main where

import AdventOfCode (runDay)
import Control.Arrow ((>>>))
import Control.Lens (over)
import Control.Monad (forM_, when)
import Control.Monad.Freer (Eff, Members, run)
import Control.Monad.Freer.State (State, evalState, get, modify)
import Data.Char (ord)
import Data.Generics.Product (field)
import Data.Graph.Inductive (Gr, Graph (labNodes, mkGraph), LEdge, LNode, prettify, sp)
import Data.List (singleton)
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import MegaparsecExtras (Parser, parseOrFail)
import SquareGrid (coordIdentifier)
import qualified SquareGrid as SG
import Text.Megaparsec (sepEndBy, some)
import Text.Megaparsec.Char (letterChar, newline)

parseLine :: Parser (V.Vector Char)
parseLine = V.fromList <$> some letterChar

parse :: String -> SG.Grid Char
parse =
  parseOrFail $
    V.fromList
      <$> parseLine `sepEndBy` newline

data GraphBlueprint = GraphBlueprint
  { nodes :: [LNode Char],
    edges :: [LEdge Int]
  }
  deriving (Generic)

asElevation :: Char -> Char
asElevation 'S' = 'a'
asElevation 'E' = 'z'
asElevation c = c

reachable :: Char -> Char -> Bool
reachable (asElevation -> src) (asElevation -> dst) =
  ord dst <= ord src + 1

computeGraph ::
  Members '[State GraphBlueprint] effs =>
  SG.Grid Char ->
  Eff effs (Gr Char Int)
computeGraph grid = do
  let nodeId = coordIdentifier grid
  forM_ (SG.allCoordsLineByLine grid) $ \thisCoord@(l, c) -> do
    let thisNode = grid SG.! thisCoord
    modify @GraphBlueprint $ over (field @"nodes") ((nodeId thisCoord, thisNode) :)
    forM_ (SG.futureFourWayNeighbors grid thisCoord) $ \thatCoord -> do
      let futureNeighbor = grid SG.! thatCoord
      when (reachable thisNode futureNeighbor) $ do
        modify @GraphBlueprint $
          over
            (field @"edges")
            ((nodeId thisCoord, nodeId thatCoord, 1) :)
      when (reachable futureNeighbor thisNode) $ do
        modify @GraphBlueprint $
          over
            (field @"edges")
            ((nodeId thatCoord, nodeId thisCoord, 1) :)
  GraphBlueprint ns es <- get
  return $ mkGraph ns es

solvePart1 :: String -> IO String
solvePart1 s = do
  let grid = parse s
  let graph = run $ evalState (GraphBlueprint [] []) $ computeGraph grid
  -- putStrLn $ prettify graph
  let (startNode, _) = head $ filter ((== 'S') . snd) $ labNodes graph
  let (finalNode, _) = head $ filter ((== 'E') . snd) $ labNodes graph
  -- print startNode
  -- print finalNode
  let shortestPath = fromJust $ sp startNode finalNode graph
  -- print $ shortestPath
  pure $ show $ length shortestPath - 1

solvePart2 :: String -> IO String
solvePart2 s = do
  let grid = parse s
  let graph = run $ evalState (GraphBlueprint [] []) $ computeGraph grid
  let startNodes = filter ((`elem` ['S', 'a']) . snd) $ labNodes graph
  let (finalNode, _) = head $ filter ((== 'E') . snd) $ labNodes graph
  let shortestPaths = catMaybes $ map (\ (sn, _) -> sp sn finalNode graph) startNodes
  pure $ show $ minimum (map length shortestPaths) - 1

main :: IO ()
main = runDay 12 solvePart1 solvePart2

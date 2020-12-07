{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative (Alternative (some))
import Data.Foldable (asum)
import Data.Functor (void, ($>))
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (__i)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, sepBy, sepEndBy, try, (<|>))
import Text.Megaparsec.Char (digitChar, letterChar, newline, space, string)

type Parser = Parsec Void String

type BagColor = String

-- | Parses a sentence like "light red bag," or "dark blue bag." and returns the
-- list of color terms.
-- >>> parseMaybe parseColoredBag "light red bags"
-- Just "light red"
parseColoredBag :: Parser String
parseColoredBag =
  do
    adjective <- some letterChar <* space
    colorName <- some letterChar <* space
    void (try (string "bags" <|> string "bag"))
    -- void (string "bag" >> try (string "s"))
    return (unwords [adjective, colorName])

-- >>> parseMaybe parseNumberedBag "1 bright white bags"
-- Just ("bright white",1)
parseNumberedBag :: Parser (BagColor, Int)
parseNumberedBag =
  do
    count <- read <$> some digitChar <* space
    bagColor <- parseColoredBag
    return (bagColor, count)

-- >>> parseMaybe parseNumberedBags "1 bright white bag, 2 muted yellow bags."
-- Just [("bright white",1),("muted yellow",2)]
parseNumberedBags :: Parser [(BagColor, Int)]
parseNumberedBags = (parseNumberedBag `sepBy` string ", ") <* string "."

data ContainmentRule
  = ContainsNoOtherBag
  | Contains [(BagColor, Int)]
  deriving (Show)

-- >>> parseMaybe parseContainmentRule "light red bags contain 1 bright white bag, 2 muted yellow bags."
-- Right ("light red",Contains [("bright white",1),("muted yellow",2)])
parseContainmentRule :: Parser (BagColor, ContainmentRule)
parseContainmentRule =
  do
    outerBag <- parseColoredBag
    void (string " contain ")
    asum
      [ try (string "no other bags." $> (outerBag, ContainsNoOtherBag)),
        do
          innerBags <- parseNumberedBags
          return (outerBag, Contains innerBags)
      ]

testInput :: String
testInput =
  [__i|
light red bags contain 1 bright white bag, 2 muted yellow bags.
bright white bags contain 1 shiny gold bag.
dotted black bags contain no other bags.
|]

type Rules = [(BagColor, ContainmentRule)]

-- >>> parseMaybe parseContainmentRules testInput
-- Just [("light red",Contains [("bright white",1),("muted yellow",2)]),("bright white",Contains [("shiny gold",1)]),("dotted black",ContainsNoOtherBag)]
parseContainmentRules :: Parser Rules
parseContainmentRules = parseContainmentRule `sepEndBy` newline

canHold :: BagColor -> Rules -> BagColor -> Bool
canHold targetColor rules color = maybe False checkRule (lookup color rules)
  where
    checkRule :: ContainmentRule -> Bool
    checkRule ContainsNoOtherBag = False
    checkRule (Contains numberedBags)
      | any ((==) targetColor . fst) numberedBags = True
      | otherwise = or (canHold targetColor rules . fst <$> numberedBags)

countBagsThatCanHold :: BagColor -> Rules -> Int
countBagsThatCanHold targetColor rules =
  let bagsToCount = canHold targetColor rules . fst <$> rules
   in length . filter (True ==) $ bagsToCount

countBagsInBag :: BagColor -> Rules -> Int
countBagsInBag color rules = maybe 0 countForRule (lookup color rules)
  where
    countForRule :: ContainmentRule -> Int
    countForRule ContainsNoOtherBag = 0
    countForRule (Contains list) = sum (countForBag <$> list)

    countForBag :: (BagColor, Int) -> Int
    countForBag (bagColor, bagCount) =
      bagCount * (1 + countBagsInBag bagColor rules)

-- >>> countBagsInBag "shiny gold" rules1
-- 32
rules1 :: Rules
rules1 =
  [ ("faded blue", ContainsNoOtherBag),
    ("dotted black", ContainsNoOtherBag),
    ("vibrant plum", Contains [("faded blue", 5), ("dotted black", 6)]),
    ("dark olive", Contains [("faded blue", 3), ("dotted black", 4)]),
    ("shiny gold", Contains [("dark olive", 1), ("vibrant plum", 2)])
  ]

main :: IO ()
main =
  do
    contents <- readFile "day07/input/real.txt"
    let rules = fromMaybe [] (parseMaybe parseContainmentRules contents)
    putStrLn [__i|Problem 1: #{countBagsThatCanHold "shiny gold" rules}|]
    putStrLn [__i|Problem 2: #{countBagsInBag "shiny gold" rules}|]

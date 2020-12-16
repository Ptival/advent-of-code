{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import AdventOfCode.Between (isBetween)
import Control.Lens (makeLenses, (^.))
import Data.Algebra.Boolean (xor)
import Data.Either (rights)
import Data.List.Utils (countElem)
import Data.String.Interpolate (i)
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, runParser)
import Text.Megaparsec.Char (asciiChar, char, digitChar, space)

data Input = Input
  { _firstNumber :: Int,
    _secondNumber :: Int,
    _password :: String,
    _wantedChar :: Char
  }
  deriving (Show)

makeLenses ''Input

type Parser = Parsec Void String

integer :: Parser Int
integer = read <$> many digitChar

parseInputLine :: Parser Input
parseInputLine =
  do
    _firstNumber <- integer <* char '-'
    _secondNumber <- integer <* space
    _wantedChar <- asciiChar <* char ':' <* space
    _password <- many asciiChar
    return $
      Input
        { _firstNumber,
          _secondNumber,
          _wantedChar,
          _password
        }

firstPolicy :: Input -> Bool
firstPolicy input =
  let occurrences = countElem (input ^. wantedChar) (input ^. password)
   in isBetween (input ^. firstNumber) (input ^. secondNumber) occurrences

secondPolicy :: Input -> Bool
secondPolicy input =
  let firstCharValid = charValidAtPosition (input ^. firstNumber)
      secondCharValid = charValidAtPosition (input ^. secondNumber)
   in firstCharValid `xor` secondCharValid
  where
    charValidAtPosition :: Int -> Bool
    charValidAtPosition position =
      (input ^. password) !! (position - 1) == input ^. wantedChar

solution :: (Input -> Bool) -> [String] -> Int
solution policy rawInputs = countValidInputs inputs
  where
    inputs = rights (runParser parseInputLine "day02" <$> rawInputs)
    countValidInputs = length . filter policy

main :: IO ()
main =
  do
    contents <- readFile "day02/input/real.txt"
    let rawInputs = lines contents
    putStrLn [i| Problem 1: #{solution firstPolicy rawInputs} |]
    putStrLn [i| Problem 2: #{solution secondPolicy rawInputs} |]

module MegaparsecExtras where

import Data.List (singleton)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec, optional, parseMaybe, sepEndBy, some)
import Text.Megaparsec.Char (alphaNumChar, char, newline, numberChar)

type Parser = Parsec Void String

lineSeparatedNumbers :: Parser [Integer]
lineSeparatedNumbers = parseNumber `sepEndBy` newline

lineSeparatedStrings :: Parser [String]
lineSeparatedStrings = some alphaNumChar `sepEndBy` newline

parseNumber :: Num a => Read a => Parser a
parseNumber = do
  sig <- fromMaybe "" <$> optional (singleton <$> char '-')
  num <- some numberChar
  return $ read $ sig <> num

parseOrFail :: Parser a -> String -> a
parseOrFail p = (fromMaybe (error "Failed to parse") . parseMaybe p)

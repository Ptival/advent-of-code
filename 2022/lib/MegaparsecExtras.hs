module MegaparsecExtras where

import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, sepEndBy, some)
import Text.Megaparsec.Char (newline, numberChar, alphaNumChar)

type Parser = Parsec Void String

lineSeparatedNumbers :: Parser [Integer]
lineSeparatedNumbers = (read <$> some numberChar) `sepEndBy` newline

lineSeparatedStrings :: Parser [String]
lineSeparatedStrings = some alphaNumChar `sepEndBy` newline

parseOrFail :: Parser a -> String -> a
parseOrFail p = (fromMaybe (error "Failed to parse") . parseMaybe p)

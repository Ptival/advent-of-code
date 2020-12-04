{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative (Applicative (liftA2))
import Control.Lens (Lens', makeLenses, set, view)
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.Foldable (asum)
import Data.Functor (void, ($>))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Maybe (fromMaybe, isJust)
import Data.String.Interpolate (__i)
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, many, parseMaybe, satisfy, sepBy, (<|>))
import Text.Megaparsec.Char (char, digitChar, hexDigitChar, hspace1, newline, string)
import Prelude hiding (max, min)

type Parser = Parsec Void String

data Passport modality = Passport
  { _birthYear :: modality String,
    _issueYear :: modality String,
    _expirationYear :: modality String,
    _height :: modality String,
    _hairColor :: modality String,
    _eyeColor :: modality String,
    _passportId :: modality String,
    _countryId :: Maybe String
  }

makeLenses ''Passport

instance Show (Passport Maybe) where
  show passport =
    concat
      [ "Passport",
        "\nBirth year:      ",
        maybe "None" show (view birthYear passport),
        "\nIssuance year:   ",
        maybe "None" show (view issueYear passport),
        "\nExpiration year: ",
        maybe "None" show (view expirationYear passport),
        "\nHeight:          ",
        maybe "None" show (view height passport),
        "\nHair color:      ",
        maybe "None" show (view hairColor passport),
        "\nEye color:       ",
        maybe "None" show (view eyeColor passport),
        "\nPassport ID:     ",
        maybe "None" show (view passportId passport),
        "\nCountry ID:      ",
        maybe "None" show (view countryId passport)
      ]

emptyPassport :: Passport Maybe
emptyPassport =
  Passport
    { _birthYear = Nothing,
      _issueYear = Nothing,
      _expirationYear = Nothing,
      _height = Nothing,
      _hairColor = Nothing,
      _eyeColor = Nothing,
      _passportId = Nothing,
      _countryId = Nothing
    }

-- | Validates a passport with possibly missing entries, returning a passport
-- where all mandatory entries are provably present. Fails if any mandatory
-- field is missing.  Does **not** check whether the field values are valid.
validateFieldsPresence :: Passport Maybe -> Maybe (Passport Identity)
validateFieldsPresence passport =
  Passport
    <$> (Identity <$> view birthYear passport)
    <*> (Identity <$> view issueYear passport)
    <*> (Identity <$> view expirationYear passport)
    <*> (Identity <$> view height passport)
    <*> (Identity <$> view hairColor passport)
    <*> (Identity <$> view eyeColor passport)
    <*> (Identity <$> view passportId passport)
    <*> Just (view countryId passport)

hasValidFieldsPresent :: Passport Maybe -> Bool
hasValidFieldsPresent = isJust . validateFieldsPresence

isWithin :: Int -> Int -> Int -> Bool
isWithin min max value = min <= value && value <= max

eyeColors :: [String]
eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

data LengthUnit
  = Centimeter
  | Inch

-- | Checks whether a passport has all mandatory fields, and whether these field
-- values are within their valid domain.
hasValidFieldValues :: Passport Maybe -> Bool
hasValidFieldValues passport = (and <$> checks) == Just True
  where
    -- Returns Just the results of all checks, or Nothing if some mandatory
    -- fields are missing.
    checks :: Maybe [Bool]
    checks =
      do
        checkedPassport <- validateFieldsPresence passport
        let parseFieldWith parser field = parseMaybe parser (runIdentity $ view field checkedPassport)
        sequence
          [ isWithin 1920 2002 <$> parseFieldWith (thisManyDigits 4) birthYear,
            isWithin 2010 2020 <$> parseFieldWith (thisManyDigits 4) issueYear,
            isWithin 2020 2030 <$> parseFieldWith (thisManyDigits 4) expirationYear,
            parseFieldWith validHeight height,
            parseFieldWith validHairColor hairColor,
            parseFieldWith validEyeColor eyeColor,
            parseFieldWith (thisManyDigits 9) passportId >> pure True
          ]

    thisManyDigits :: Int -> Parser Int
    thisManyDigits howMany = read <$> replicateM howMany digitChar <* eof

    validHeight :: Parser Bool
    validHeight =
      do
        numericPart <- read <$> many digitChar
        unit <- (string "cm" $> Centimeter) <|> (string "in" $> Inch)
        return (checkHeight unit numericPart)

    checkHeight :: LengthUnit -> Int -> Bool
    checkHeight Centimeter = isWithin 150 193
    checkHeight Inch = isWithin 59 76

    validHairColor :: Parser Bool
    validHairColor = char '#' >> replicateM 6 hexDigitChar >> eof >> pure True

    validEyeColor :: Parser Bool
    validEyeColor = asum (string <$> eyeColors) >> pure True

-- | Parses a passport field and adds its information to some pre-existing
-- passport information.
passportField :: Passport Maybe -> Parser (Passport Maybe)
passportField currentPassport =
  asum
    [ setField birthYear <$> (string "byr:" *> fieldCharacters),
      setField issueYear <$> (string "iyr:" *> fieldCharacters),
      setField expirationYear <$> (string "eyr:" *> fieldCharacters),
      setField height <$> (string "hgt:" *> fieldCharacters),
      setField hairColor <$> (string "hcl:" *> fieldCharacters),
      setField eyeColor <$> (string "ecl:" *> fieldCharacters),
      setField passportId <$> (string "pid:" *> fieldCharacters),
      setField countryId <$> (string "cid:" *> fieldCharacters)
    ]
  where
    fieldCharacters :: Parser String
    fieldCharacters = many (satisfy (not . isSpace))
    setField :: Lens' (Passport Maybe) (Maybe a) -> a -> Passport Maybe
    setField field value = set field (Just value) currentPassport

-- >>> parseMaybe (passportField emptyPassport) "ecl:4242"
-- Just Passport
-- Birth year:      None
-- Issuance year:   None
-- Expiration year: None
-- Height:          None
-- Hair color:      None
-- Eye color:       "4242"
-- Passport ID:     None
-- Country ID:      None

passportEntry :: Parser (Passport Maybe)
passportEntry = restOfPassportEntry emptyPassport
  where
    restOfPassportEntry :: Passport Maybe -> Parser (Passport Maybe)
    restOfPassportEntry currentPassport =
      (passportField currentPassport <* consumeSpace)
        >>= liftA2 (<|>) restOfPassportEntry pure
    consumeSpace :: Parser ()
    consumeSpace = hspace1 <|> void newline <|> pure ()

-- >>> parseMaybe passportEntry "ecl:4242   byr:1234\ncid:3245"
-- Just Passport
-- Birth year:      "1234"
-- Issuance year:   None
-- Expiration year: None
-- Height:          None
-- Hair color:      None
-- Eye color:       "4242"
-- Passport ID:     None
-- Country ID:      "3245"

passportEntries :: Parser [Passport Maybe]
passportEntries = sepBy passportEntry newline

readPassports :: String -> [Passport Maybe]
readPassports = fromMaybe [] . parseMaybe passportEntries

solveProblem1 :: [Passport Maybe] -> Int
solveProblem1 = length . filter hasValidFieldsPresent

solveProblem2 :: [Passport Maybe] -> Int
solveProblem2 = length . filter hasValidFieldValues

invalidPassports :: String
invalidPassports =
  [__i|
eyr:1972 cid:100
hcl:\#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:\#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007
|]

-- >>> solveProblem2 (readPassports invalidPassports) == 0
-- True

validPassports :: String
validPassports =
  [__i|
pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:\#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:\#a97842 hgt:165cm

hcl:\#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:\#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
|]

-- >>> solveProblem2 (readPassports validPassports) == 4
-- True

main :: IO ()
main =
  do
    contents <- readFile "day04/input/real.txt"
    let passports = readPassports contents
    putStrLn [__i|Problem 1: #{solveProblem1 passports}|]
    putStrLn [__i|Problem 2: #{solveProblem2 passports}|]

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import AdventOfCode (runDay)
import Control.Applicative (Alternative (many, some), asum)
import Control.Arrow ((>>>))
import Data.List (findIndex, foldl')
import Data.Tuple.Extra ((&&&))
import MegaparsecExtras (Parser, parseNumber, parseOrFail)
import Text.Megaparsec (MonadParsec (try), sepEndBy)
import Text.Megaparsec.Char (char, newline, printChar, string)

data LsOutput
  = LSDir String
  | LSFile Int String
  deriving (Show)

parseLsOutput :: Parser LsOutput
parseLsOutput =
  try $
    asum
      [ LSDir <$> (string "dir " *> some printChar),
        LSFile <$> (parseNumber <* char ' ') <*> some printChar
      ]

data Cmd
  = Cd String
  | Ls [LsOutput]
  deriving (Show)

parseLine :: Parser Cmd
parseLine =
  try $
    asum
      [ Cd <$> (string "$ cd " *> some printChar <* newline),
        do
          _ <- string "$ ls" >> newline
          Ls <$> parseLsOutput `sepEndBy` newline
      ]

parse :: String -> [Cmd]
parse =
  parseOrFail $
    many parseLine

data FileSystem
  = FSDir String [FileSystem]
  | FSFile Int String
  deriving (Show)

data FileSystemZipper
  = AtFSDir
      -- parent directories
      [ ( String, -- parent dir name
          [FileSystem], -- items to my left in my parent's children
          [FileSystem] -- items to my right in my parent's children
        )
      ]
      String
      [FileSystem]
  deriving (Show)

isDir :: String -> FileSystem -> Bool
isDir tgt (FSDir dir _) = tgt == dir
isDir _ _ = False

asFS :: LsOutput -> FileSystem
asFS (LSFile sz nm) = FSFile sz nm
asFS (LSDir nm) = FSDir nm []

navigateUp :: FileSystemZipper -> FileSystemZipper
navigateUp (AtFSDir ((parentName, toMyLeft, toMyRight) : rest) myName myChildren) =
  AtFSDir rest parentName (toMyLeft ++ [FSDir myName myChildren] ++ toMyRight)
navigateUp _ = error "Cannot .. from root directory"

atRoot :: FileSystemZipper -> Bool
atRoot (AtFSDir [] _ _) = True
atRoot _ = False

navigateUpToRoot :: FileSystemZipper -> FileSystemZipper
navigateUpToRoot = until atRoot navigateUp

runCmd :: Cmd -> FileSystemZipper -> FileSystemZipper
runCmd (Cd "/") zipper = zipper -- TODO
runCmd (Cd "..") z = navigateUp z
runCmd (Cd dirName) (AtFSDir parents curName curChildren) =
  let Just index = findIndex (isDir dirName) curChildren
      FSDir _ dirChildren = curChildren !! index
      toMyLeft = take index curChildren
      toMyRight = drop (index + 1) curChildren
   in AtFSDir ((curName, toMyLeft, toMyRight) : parents) dirName dirChildren
runCmd (Ls entries) (AtFSDir parents curName _) = AtFSDir parents curName (map asFS entries)

size :: FileSystem -> Int
size (FSDir _ children) = sum $ map size children
size (FSFile fileSize _) = fileSize

asFileSystem :: FileSystemZipper -> FileSystem
asFileSystem (AtFSDir [] _ fs) = FSDir "/" fs
asFileSystem _ = error "nope"

allFileSystems :: FileSystem -> [FileSystem]
allFileSystems fs@(FSDir _ children) = fs : concatMap allFileSystems children
allFileSystems (FSFile _ _) = []

initialFileSystemZipper :: FileSystemZipper
initialFileSystemZipper = AtFSDir [] "/" []

computeAllFileSystems :: String -> [FileSystem]
computeAllFileSystems =
  parse
    >>> foldl' (flip runCmd) initialFileSystemZipper
    >>> navigateUpToRoot
    >>> asFileSystem
    >>> allFileSystems

solvePart1 :: String -> IO String
solvePart1 =
  computeAllFileSystems
    >>> map size
    >>> filter (<= 100_000)
    >>> sum
    >>> pure . show

keepIfFreesEnoughSpace :: Int -> [Int] -> [Int]
keepIfFreesEnoughSpace currentUsedSpace = filter freesEnoughSpace
  where
    freesEnoughSpace dirSize = 70_000_000 - currentUsedSpace + dirSize >= 30_000_000

solvePart2 :: String -> IO String
solvePart2 =
  computeAllFileSystems
    >>> map size
    >>> (maximum &&& id)
    >>> uncurry keepIfFreesEnoughSpace
    >>> minimum
    >>> pure . show

main :: IO ()
main = runDay 07 solvePart1 solvePart2

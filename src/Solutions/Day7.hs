{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Solutions.Day7 where

import           Common.AoCSolutions      (AoCSolution (MkAoCSolution),
                                           printSolutions, printTestSolutions)
import           Control.Applicative      (Alternative ((<|>)))
import           Control.Lens             (makeLenses)
import           Control.Lens.Extras      (biplate)
import           Data.Foldable            (Foldable (foldl'))
import           Data.Function            ((&))
import           Data.Functor             (($>))
import           Data.Functor.Foldable.TH (MakeBaseFunctor (makeBaseFunctor))
import qualified Data.Map                 as M
import           Data.Tree                (Tree (Node))
import           Text.Trifecta            (CharParsing (char, string), Parser,
                                           Parsing (try), TokenParsing (token),
                                           choice, integer, letter, newline,
                                           some, space)
import Data.List (intersperse, tails)
import GHC.OldList (intercalate)


data OutputLine
  = Command !Cmd
  | Dir !String
  | FileSize
      { name :: !String
      , size :: !Integer
      }
  deriving (Eq, Show)

data Cmd
  = CD !String
  | LS
  deriving (Eq, Show)

aoc7 :: IO ()
aoc7 = do
  printSolutions 7 $ MkAoCSolution parseInput part1
  printSolutions 7 $ MkAoCSolution parseInput part2

parseInput :: Parser [OutputLine]
parseInput = some $ token parseLine

parseLine :: Parser OutputLine
parseLine = do
  choice $ map try [cd, ls, dir, file]
  where cd :: Parser OutputLine
        cd = do
          char '$'
          space
          name <- string "cd " *> some (choice [letter, char '/', char '.'])
          pure $ Command $ CD name
        ls :: Parser OutputLine
        ls = char '$' *> string " ls" $> Command LS
        dir :: Parser OutputLine
        dir = do
          name <- string "dir " *> some letter --let's assume directories are just letters
          pure $ Dir name
        file :: Parser OutputLine
        file = do
          size <- integer
          name <- some (letter <|> char '.')
          pure $ FileSize name size

data RunningTotal
  = MkRunningTotal
      { sizes          :: !(M.Map String Integer)
      , directoryStack :: ![String]
      }
  deriving (Eq, Show)


part1 :: [OutputLine] -> Integer
part1 outputLines = M.foldr (+) 0 dirSizes
  where filtered = M.filter (<=100000) dirSizes
        dirSizes = getDirectorySizes outputLines

part2 :: [OutputLine] -> Integer
part2 outputLines = minimum $ map snd $ M.toList $ M.filter (>= spaceToFree) dirSizes
  where dirSizes = getDirectorySizes outputLines
        usedSpace = dirSizes M.! "/"
        unusedSpace = 70000000 - usedSpace
        spaceToFree = 30000000 - unusedSpace

getDirectorySizes :: [OutputLine] -> M.Map String Integer
getDirectorySizes = sizes . foldl' foldInstruction (MkRunningTotal M.empty [])

foldInstruction :: RunningTotal -> OutputLine -> RunningTotal
foldInstruction rt@MkRunningTotal{..} ol = case ol of
  Command cmd -> case cmd of
    CD dirName -> changeDirectory dirName
    LS         -> rt
  Dir dirName -> rt
  FileSize _ size -> rt { sizes = updateSizes size }
  where changeDirectory dirName
              | dirName == ".." = rt { directoryStack = tail directoryStack }
              | otherwise = rt { directoryStack = dirName : directoryStack }
        updateSizes size = let paths = map constructFilePath $ tails directoryStack
                           in foldr (\path sizesAccum -> M.insertWith (+) path size sizesAccum) sizes paths
        constructFilePath dirNames = intercalate "/" dirNames


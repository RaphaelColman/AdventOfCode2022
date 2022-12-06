module Solutions.Day6
    ( aoc6
    ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.ListUtils    (windowN)
import           Data.Foldable       (find)
import           Data.List           (nub)
import           GHC.OldList         (findIndex)
import           Text.Trifecta       (CharParsing (anyChar), Parser, manyTill,
                                      newline, some)

aoc6 :: IO ()
aoc6 = do
  printSolutions 6 $ MkAoCSolution parseInput part1
  printSolutions 6 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = manyTill anyChar newline

part1 :: [Char] -> Maybe Int
part1 = findMarker 4

part2 :: [Char] -> Maybe Int
part2 = findMarker 14

findMarker :: Eq a => Int -> [a] -> Maybe Int
findMarker amount input = (+amount) <$> findIndex allDifferent windows
  where windows = windowN amount input
        allDifferent xs = length (nub xs) == length xs

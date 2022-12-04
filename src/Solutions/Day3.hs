module Solutions.Day3
    ( aoc3
    ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Data.Char           (isUpper, ord)
import           Data.List           (intersect, nub)
import           Data.List.Split     (chunksOf)
import qualified Data.Set            as S
import           Text.Trifecta       (CharParsing (anyChar, string), Parser,
                                      TokenParsing (token), letter, some)

aoc3 :: IO ()
aoc3 = do
  printSolutions 3 $ MkAoCSolution parseInput part1
  printSolutions 3 $ MkAoCSolution parseInput part2

type Rucksack = String

parseInput :: Parser [Rucksack]
parseInput = some $ token $ some letter

part1 :: [Rucksack] -> Integer
part1 = sum . map (charValue . commonLetter)

part2 :: [Rucksack] -> Integer
part2 = sum . map charValue . badges

commonLetter :: Rucksack -> Char
commonLetter rucksack = if length commonLetters == 1
                        then head commonLetters
                        else error "More than one common letter"
  where (p1, p2) = splitAt (length rucksack `div` 2) rucksack
        commonLetters = nub $ p1 `intersect` p2

--ASCII A = 65 (so always subtract 38 to make it 27)
--ASCII a = 97 (so always subtract 96 to make it 1)
charValue :: Char -> Integer
charValue c
  | isUpper c = toInteger $ ord c - 38
  | otherwise = toInteger $ ord c - 96

badges :: [Rucksack] -> String
badges rucksacks = map head intersections
  where groups = chunksOf 3 rucksacks
        intersections = map (\[s1, s2, s3] -> s1 `intersect` s2 `intersect` s3) groups

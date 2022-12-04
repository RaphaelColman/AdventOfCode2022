module Solutions.Day3
    ( aoc3
    ) where

import           Common.AoCSolutions  (AoCSolution (MkAoCSolution),
                                       printSolutions, printTestSolutions)
import           Common.FunctorUtils  (fmap2)
import           Control.Monad        (guard, unless)
import           Control.Monad.Except (MonadError (throwError))
import           Data.Char            (isUpper, ord)
import           Data.List            (intersect, nub)
import           Data.List.Split      (chunksOf)
import qualified Data.Set             as S
import           Text.Trifecta        (CharParsing (anyChar, string), Parser,
                                       TokenParsing (token), letter, some)

aoc3 :: IO ()
aoc3 = do
  printSolutions 3 $ MkAoCSolution parseInput part1
  printSolutions 3 $ MkAoCSolution parseInput part2

type Rucksack = String

parseInput :: Parser [Rucksack]
parseInput = some $ token $ some letter

part1 :: [Rucksack] -> Either String Integer
part1 rucksacks = do
  values <- traverse (fmap charValue . commonLetter) rucksacks
  pure $ sum values

part2 :: [Rucksack] -> Either String Integer
part2 rucksacks = fmap sum $ fmap2 charValue $ badges rucksacks

commonLetter :: Rucksack -> Either String Char
commonLetter rucksack = do
  unless (even (length rucksack)) $ throwError ("Number of items is not even: " ++ rucksack)
  let (p1, p2) = splitAt (length rucksack `div` 2) rucksack
  let commonLetters = nub $ p1 `intersect` p2
  unless (length commonLetters == 1) $ throwError ("More than one common letter found: " ++ rucksack)
  pure $ head commonLetters

--ASCII A = 65 (so always subtract 38 to make it 27)
--ASCII a = 97 (so always subtract 96 to make it 1)
charValue :: Char -> Integer
charValue c
  | isUpper c = toInteger $ ord c - 38
  | otherwise = toInteger $ ord c - 96

badges :: [Rucksack] -> Either String [Char]
badges rucksacks = do
  let groups = chunksOf 3 rucksacks
  unless (all ((==3) . length) groups) $ throwError "Total number of rucksacks is not divisible by 3"
  let intersections = map (nub . \[s1, s2, s3] -> s1 `intersect` s2 `intersect` s3) groups
  unless (all ((==1) . length) intersections) $ throwError ("More than one common item in groups: " ++ show intersections)
  pure $ map head intersections

{-# LANGUAGE RecordWildCards #-}
module Solutions.Day4
    ( aoc4
    ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Data.List           (intersect)
import qualified Data.Set as S
import           Text.Trifecta       (CharParsing (char), Parser, charLiteral,
                                      commaSep, integer, some)
import qualified Data.Semigroup as S
type Range = (Integer, Integer)
data ElfPair
  = MkElfPair
      { elf1 :: !Range
      , elf2 :: !Range
      }
  deriving (Eq, Show, Ord)

aoc4 :: IO ()
aoc4 = do
  printSolutions 4 $ MkAoCSolution parseInput part1
  printSolutions 4 $ MkAoCSolution parseInput part2

parseInput :: Parser [ElfPair]
parseInput = some parseElfPair

parseElfPair :: Parser ElfPair
parseElfPair = do
  [e1, e2] <- commaSep parseElf
  pure $ MkElfPair e1 e2
  where parseElf :: Parser (Integer, Integer)
        parseElf = do
          fst <- integer <* char '-'
          snd <- integer
          pure (fst, snd)

part1 :: [ElfPair] -> Int
part1 = length . filter isRedundant

part2 :: [ElfPair] -> Int
part2 = length . filter overlaps

contains :: Range -> Range -> Bool
contains (l1, u1) (l2, u2) = l1 <= l2 && u1 >= u2

isRedundant :: ElfPair -> Bool
isRedundant MkElfPair{..} = elf1 `contains` elf2 || elf2 `contains` elf1

overlaps :: ElfPair -> Bool
overlaps ep@(MkElfPair (l1, u1) (l2, u2)) = (l1 >= l2 && l1 <= u2)
                                            || (u1 <= u2 && u1 >= l2)
                                            || isRedundant ep

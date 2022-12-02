module Solutions.Day1
    ( aoc1
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Common.ListUtils        (window2, window3)
import           Control.Applicative     ((<|>))
import           Control.Monad.Extra     (skip)
import           Data.Functor            (($>))
import           Data.List               (sort, tails)
import           Text.Parser.Combinators (Parsing (eof), skipOptional)
import           Text.Trifecta           (CharParsing (anyChar), Parser,
                                          TokenParsing (token), integer,
                                          integer', newline, some, whiteSpace)

aoc1 :: IO ()
aoc1 = do
  printSolutions 1 $ MkAoCSolution parseInput part1
  printSolutions 1 $ MkAoCSolution parseInput part2

type Elf = [Integer]
type Elves = [Elf]

parseInput :: Parser Elves
parseInput = do
  some $ parseElf <* (skipOptional newline <|> skipOptional eof)
  where parseElf :: Parser [Integer]
        parseElf = some $ integer' <* newline

part1 :: Elves -> Integer
part1 = maximum . map sum

part2 :: Elves -> Integer
part2 = sum . 
        take 3 .
        reverse .
        sort .
        map sum

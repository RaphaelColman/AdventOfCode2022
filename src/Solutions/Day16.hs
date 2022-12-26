module Solutions.Day16
    ( aoc16
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions)
import           Control.Applicative     (many, some, (<|>))
import           Data.Functor            (($>))
import           Text.Parser.Combinators (Parsing (try))
import           Text.Trifecta           (CharParsing (string), Parser, integer)

aoc16 :: IO ()
aoc16 = do
  printSolutions 16 $ MkAoCSolution parseInput part1
  --printSolutions 16 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = do
  pure undefined
part1 = id


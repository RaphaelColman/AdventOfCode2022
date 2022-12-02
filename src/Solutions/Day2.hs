module Solutions.Day2
    ( aoc2
    ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.EnumUtils    (enumNext, enumPrev)
import           Text.Trifecta       (CharParsing (anyChar), Parser,
                                      TokenParsing (token), some, whiteSpace)

aoc2 :: IO ()
aoc2 = do
  printSolutions 2 $ MkAoCSolution parseInput part1
  printSolutions 2 $ MkAoCSolution parseInput part2

data Shape = Rock | Paper | Scissors deriving (Bounded, Enum, Eq, Ord, Show)
type Game = (Shape, Shape)
data GameResult = Win | Draw | Loss deriving (Bounded, Enum, Eq, Ord, Show)

parseInput :: Parser [Game]
parseInput = do
  some $ token parseGame

parseGame :: Parser (Shape, Shape)
parseGame = do
  p1 <- anyChar >>= shapeOf
  whiteSpace
  p2 <- anyChar >>= shapeOf
  pure (p1, p2)
  where shapeOf c = case c of
          'A' -> pure Rock
          'B' -> pure Paper
          'C' -> pure Scissors
          'X' -> pure Rock
          'Y' -> pure Paper
          'Z' -> pure Scissors
          _   -> fail "Unrecognised input"

part1 :: [Game] -> Integer
part1 = sum . map scoreGame

part2 :: [Game] -> Integer
part2 = sum . map (scoreGame . convertGame)

calculateResult :: Game -> GameResult
calculateResult (p1, p2)
    | p1 == p2 = Draw
    | p2 == enumNext p1 = Win
    | otherwise = Loss

scoreGame :: Game -> Integer
scoreGame game@(p1, p2) = toInteger $ gameScore + shapeScore
  where shapeScore = 1 + fromEnum p2
        gameScore = case calculateResult game of
          Win  -> 6
          Draw -> 3
          Loss -> 0

convertGame :: Game -> Game
convertGame (p1, p2) = case expectedResult of
  Win  -> (p1, enumNext p1)
  Draw -> (p1, p1)
  Loss -> (p1, enumPrev p1)
  where expectedResult = case p2 of
          Rock     -> Loss
          Paper    -> Draw
          Scissors -> Win

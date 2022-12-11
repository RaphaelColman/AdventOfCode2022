{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solutions.Day10
    ( aoc10
    ) where

import           Combinatorics.Mastermind (Eval (white))
import           Common.AoCSolutions      (AoCSolution (MkAoCSolution),
                                           printSolutions, printTestSolutions)
import           Control.Applicative      (some, (<|>))
import           Data.Foldable            (Foldable (toList), foldrM)
import           Data.Functor             (($>))
import qualified Data.Sequence            as S
import           Text.Parser.Combinators  (try)
import           Text.Trifecta            (CharParsing (anyChar, char, string),
                                           Parser, commaSep, integer, newline,
                                           space, whiteSpace)
import Control.Monad.State (runState)
import Control.Monad.Trans.State (modify)
import Data.List (foldl')

aoc10 :: IO ()
aoc10 = do
  printTestSolutions 10 $ MkAoCSolution parseInput part1
  --printSolutions 10 $ MkAoCSolution parseInput part2

data SimianState
  = MkState
      { _monkeys      :: !(S.Seq Monkey)
      , _currentIndex :: !Integer
      }
  deriving (Show)

data Monkey
  = MkMonkey
      { _items       :: !(S.Seq Integer)
      , _operation   :: !(Integer -> Integer)
      , _test        :: !Integer --Divisible by this number
      , _trueMonkey  :: !Integer
      , _falseMonkey :: !Integer
      }
  deriving (Show)

instance Show (Integer -> Integer) where
  show f = "Some function"

parseInput :: Parser (S.Seq Monkey)
parseInput = S.fromList <$> some parseMonkey

parseMonkey :: Parser Monkey
parseMonkey = do
  string "Monkey " >> integer >> char ':' >> newline >> whiteSpace
  string "Starting items: "
  startingItems <- S.fromList <$> commaSep integer
  expression <- parseExpression
  whiteSpace
  string "Test: divisible by "
  test <- integer
  string "If true: throw to monkey "
  trueMonkey <- integer
  string "If false: throw to monkey "
  MkMonkey startingItems expression test trueMonkey <$> integer

parseExpression :: Parser (Integer -> Integer)
parseExpression = do
  string "Operation: new = old "
  exprCh <- anyChar
  space
  i <- try (Just <$> integer) <|> try (string "old" $> Nothing)
  expr :: Integer -> Integer -> Integer <- case exprCh of
        '*' -> pure (*)
        '+' -> pure (+)
        _   -> fail "unexpected char"
  case i of
    Nothing -> pure (\x -> x `expr` x)
    Just n  -> pure (`expr` n)

part1 = doRound . initState

initState :: S.Seq Monkey -> SimianState
initState = flip MkState 0

stepState :: SimianState -> Maybe SimianState
stepState state@MkState{..} = do
  current <- S.lookup (fromInteger _currentIndex) _monkeys
  let destinations = processItems current
  pure $ snd $ flip runState state $ do
      modify clearCurrent
      modify $ flip (foldl' go) destinations --actually the order DOES matter here
      modify incrementIndex
  where go :: SimianState -> (Integer, Integer) -> SimianState
        go state'@(MkState monkeys _ ) (worryLevel, destination) = let newMonkeys = S.adjust (appendToMonkeyList worryLevel) (fromInteger destination) monkeys
                                                                  in state' { _monkeys = newMonkeys }
        clearCurrent :: SimianState -> SimianState
        clearCurrent s@(MkState monkeys _) = s { _monkeys = S.adjust clearMonkey (fromInteger _currentIndex) monkeys }
        incrementIndex s@(MkState _ i) = s { _currentIndex = i+1 }

doRound :: SimianState -> SimianState
doRound ss = maybe ss doRound (stepState ss)

-- |Returns a list of (worryLevel, destination) for each item
processItems :: Monkey -> [(Integer, Integer)]
processItems MkMonkey{..} = map go (toList _items)
  where go item = let newWorryLevel = (_operation item `div` 3)
                  in if newWorryLevel `mod` _test == 0 then (newWorryLevel, _trueMonkey) else (newWorryLevel, _falseMonkey)

isDivisibleBy :: Integer -> Integer -> Bool
isDivisibleBy x y = x `mod` y == 0

appendToMonkeyList :: Integer -> Monkey -> Monkey
appendToMonkeyList i m@MkMonkey{..} = m { _items = _items S.|> i }

clearMonkey :: Monkey -> Monkey
clearMonkey m@MkMonkey{..} = m { _items = S.empty }

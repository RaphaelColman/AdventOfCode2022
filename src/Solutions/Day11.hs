{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solutions.Day11
    ( aoc11
    ) where

import           Combinatorics.Mastermind  (Eval (white))
import           Common.AoCSolutions       (AoCSolution (MkAoCSolution),
                                            printSolutions, printTestSolutions)
import           Control.Applicative       (some, (<|>))
import           Control.Monad.State       (runState)
import           Control.Monad.Trans.State (modify)
import           Data.Foldable             (Foldable (toList), foldrM)
import qualified Data.Foldable             as M
import           Data.Functor              (($>))
import qualified Data.IntMap               as IM
import           Data.List                 (foldl')
import qualified Data.Sequence             as S
import           GHC.OldList               (sortOn)
import           Text.Parser.Combinators   (try)
import           Text.Trifecta             (CharParsing (anyChar, char, string),
                                            Parser, commaSep, integer, newline,
                                            space, whiteSpace)
import qualified Data.Ord

aoc11 :: IO ()
aoc11 = do
  printSolutions 11 $ MkAoCSolution parseInput part1
  --printSolutions 11 $ MkAoCSolution parseInput part2

data SimianState
  = MkState
      { _monkeys      :: !(S.Seq Monkey)
      , _currentIndex :: !Integer
      , _countedItems :: !(IM.IntMap Integer)
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

part1 :: S.Seq Monkey -> Integer
part1 = solve

solve :: S.Seq Monkey -> Integer
solve state = product $ map snd bestMonkeys
  where run = runMonkeys 20 state
        bestMonkeys = take 2 $ sortOn (Data.Ord.Down . snd) (IM.toList $ _countedItems run)

runMonkeys :: Int -> S.Seq Monkey -> SimianState
runMonkeys times state = iterate doRound (initState state) !! times

initState :: S.Seq Monkey -> SimianState
initState monkeys = MkState monkeys 0 IM.empty

stepState :: SimianState -> Maybe SimianState
stepState state@MkState{..} = do
  current <- S.lookup (fromInteger _currentIndex) _monkeys
  let destinations = processItems current
  pure $ snd $ flip runState state $ do
      modify clearCurrent
      modify $ flip (foldl' go) destinations
      modify $ updateCount (toInteger (length destinations))
      modify incrementIndex
  where go :: SimianState -> (Integer, Integer) -> SimianState
        go state'@(MkState monkeys _ _) (worryLevel, destination) = let newMonkeys = S.adjust (appendToMonkeyList worryLevel) (fromInteger destination) monkeys
                                                                  in state' { _monkeys = newMonkeys }
        clearCurrent :: SimianState -> SimianState
        clearCurrent s@(MkState monkeys _ _) = s { _monkeys = S.adjust clearMonkey (fromInteger _currentIndex) monkeys }
        incrementIndex s@(MkState _ i _) = s { _currentIndex = i+1 }
        updateCount amount s = s { _countedItems = IM.insertWith (+) (fromInteger _currentIndex) amount _countedItems }

doRound :: SimianState -> SimianState
doRound ss = maybe (ss { _currentIndex = 0 }) doRound (stepState ss)

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

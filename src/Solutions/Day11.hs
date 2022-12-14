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
import qualified Data.Ord
import qualified Data.Sequence             as S
import           GHC.OldList               (sortOn)
import           Text.Parser.Combinators   (try)
import           Text.Trifecta             (CharParsing (anyChar, char, string),
                                            Parser, commaSep, integer, newline,
                                            space, whiteSpace)

aoc11 :: IO ()
aoc11 = do
  printSolutions 11 $ MkAoCSolution parseInput part1
  printSolutions 11 $ MkAoCSolution parseInput part2

data SimianState
  = MkState
      { _monkeys          :: !(S.Seq Monkey)
      , _currentIndex     :: !Integer
      , _countedItems     :: !(IM.IntMap Integer)
      , _divideWorryLevel :: !Bool
      , _roundNumber      :: !Integer
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
part1 = solve True 20

part2 :: S.Seq Monkey -> Integer
part2 = solve False 10000

solve :: Bool -> Integer -> S.Seq Monkey -> Integer
solve divideBy3 times state = product $ map snd bestMonkeys
  where run = runMonkeys divideBy3 (fromInteger times) state
        bestMonkeys = take 2 $ sortOn (Data.Ord.Down . snd) (IM.toList $ _countedItems run)

runMonkeys :: Bool -> Int -> S.Seq Monkey -> SimianState
runMonkeys divideBy3 times state = iterate doRound (initState divideBy3 state) !! times

initState :: Bool -> S.Seq Monkey -> SimianState
initState divideBy3 monkeys = MkState monkeys 0 IM.empty divideBy3 0

stepState :: SimianState -> Maybe SimianState
stepState state@MkState{..} = do
  current <- S.lookup (fromInteger _currentIndex) _monkeys
  let uniqueModulo = product $ _test <$> _monkeys
  let destinations = processItems _divideWorryLevel uniqueModulo current
  pure $ snd $ flip runState state $ do
      modify clearCurrent
      modify $ flip (foldl' go) destinations
      modify $ updateCount (toInteger (length destinations))
      modify incrementIndex
  where go :: SimianState -> (Integer, Integer) -> SimianState
        go state'@(MkState monkeys _ _ _ _) (worryLevel, destination) = let newMonkeys = S.adjust (appendToMonkeyList worryLevel) (fromInteger destination) monkeys
                                                                  in state' { _monkeys = newMonkeys }
        clearCurrent :: SimianState -> SimianState
        clearCurrent s@(MkState monkeys _ _ _ _) = s { _monkeys = S.adjust clearMonkey (fromInteger _currentIndex) monkeys }
        incrementIndex s@(MkState _ i _ _ _) = s { _currentIndex = i+1 }
        updateCount amount s = s { _countedItems = IM.insertWith (+) (fromInteger _currentIndex) amount _countedItems }

doRound :: SimianState -> SimianState
doRound ss@MkState{..} = maybe (ss { _currentIndex = 0, _roundNumber = _roundNumber + 1 }) doRound (stepState ss)

-- |Returns a list of (worryLevel, destination) for each item
processItems :: Bool -> Integer -> Monkey -> [(Integer, Integer)]
processItems divideWorryLevel uniqueModulo MkMonkey{..} = map go (toList _items)
  where go item = let newWorryLevel = if divideWorryLevel then _operation item `div` 3 else _operation item `mod` uniqueModulo
                  in if newWorryLevel `mod` _test == 0 then (newWorryLevel, _trueMonkey) else (newWorryLevel, _falseMonkey)

isDivisibleBy :: Integer -> Integer -> Bool
isDivisibleBy x y = x `mod` y == 0

appendToMonkeyList :: Integer -> Monkey -> Monkey
appendToMonkeyList i m@MkMonkey{..} = m { _items = _items S.|> i }

clearMonkey :: Monkey -> Monkey
clearMonkey m@MkMonkey{..} = m { _items = S.empty }

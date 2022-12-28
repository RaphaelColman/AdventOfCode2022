{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solutions.Day16
    ( aoc16
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Common.FunctorUtils     (fmap2)
import           Common.ListUtils        (freqs)
import           Control.Applicative     (many, some, (<|>))
import           Data.Functor            (($>))
import           Data.Graph.AStar        (aStar)
import qualified Data.HashMap.Strict     as HM
import           Data.HashSet            (HashSet)
import qualified Data.HashSet            as HS
import           Data.Hashable           (Hashable)
import           GHC.Generics            (Generic)
import           Text.Parser.Combinators (Parsing (try))
import           Text.Trifecta           (CharParsing (anyChar, char, string),
                                          Parser, TokenParsing (token),
                                          commaSep, integer, letter, space,
                                          whiteSpace)
import Debug.Trace (traceShow)

aoc16 :: IO ()
aoc16 = do
  printSolutions 16 $ MkAoCSolution parseInput part1
  --printSolutions 16 $ MkAoCSolution parseInput part2

data ValveData
  = MkValveData
      { _flowRate :: !Integer
      , _children :: ![String]
      , _on       :: !Bool
      }
  deriving (Eq, Generic, Ord, Show)

instance Hashable ValveData

--Maybe make this an actual graph?
data ValveSystem
  = MkValveSystem
      { _system        :: !(HM.HashMap String ValveData)
      , _currentValve  :: !String
      , _currentTime   :: !Integer
      , _currentFlow   :: !Integer
      , _totalReleased :: !Integer
      }
  deriving (Eq, Generic, Ord, Show)

instance Hashable ValveSystem

parseInput :: Parser (HM.HashMap String ValveData)
parseInput = do
  lines <- some $ token parseValveLine
  pure $ HM.fromList lines

parseValveLine :: Parser (String, ValveData)
parseValveLine = do
  name <- string "Valve " *> some letter
  flowrate <- string " has flow rate=" *> integer <* char ';'
  try (string " tunnels lead to") <|> try (string " tunnel leads to")
  try (string " valves ") <|> try (string " valve ")
  children <- commaSep (some letter)
  pure (name, MkValveData flowrate children False)

part1 mp = fmap2 _totalReleased $ solve system
  where system = MkValveSystem mp "AA" 0 0 0

neighbourNodes :: ValveSystem -> HashSet ValveSystem
neighbourNodes system@MkValveSystem{..} = HS.fromList $! travelValves ++ openCurrenValve --Maybe can make this faster by not concatenating lists
  where cValve@MkValveData{..} = _system HM.! _currentValve
        travelValves = map (\child -> system { _currentValve = child,
                                          _currentTime = _currentTime+1,
                                          _totalReleased = _totalReleased + _currentFlow }) _children
        openCurrenValve = let newFlow = _currentFlow + _flowRate
                          in [system { _currentTime = _currentTime+1,
                                    _system = HM.insert _currentValve cValve { _on = True } _system, --would it be faster to call HM.alter?
                                    _currentFlow = newFlow,
                                    _totalReleased = _totalReleased + _currentFlow
                                    } | not _on]

--This is still really slow. Maybe I can do this without copying the map of valves everywhere (using the state monad perhaps?)
--Or maybe I can create a better heuristic function?

cost :: ValveSystem -> ValveSystem -> Integer
cost _ MkValveSystem{..} = maxFlow - _currentFlow
  where maxFlow = HM.foldr' (\v a -> _flowRate v + a) 0 _system

--Arithmetic sequence sum: n/2[2a + (n-1)d]
heuristic :: ValveSystem -> Integer
heuristic MkValveSystem{..} = maxFlow - _currentFlow
  where maxFlow = HM.foldr' (\v a -> _flowRate v + a) 0 _system

finished :: ValveSystem -> Bool
finished MkValveSystem{..} = _currentTime == 30

solve :: ValveSystem -> Maybe [ValveSystem]
solve = aStar neighbourNodes cost heuristic finished


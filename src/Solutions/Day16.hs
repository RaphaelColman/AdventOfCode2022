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
import           Debug.Trace             (traceShow)
import           GHC.Generics            (Generic)
import           Text.Parser.Combinators (Parsing (try))
import           Text.Trifecta           (CharParsing (anyChar, char, string),
                                          Parser, TokenParsing (token),
                                          commaSep, integer, letter, space,
                                          whiteSpace)

aoc16 :: IO ()
aoc16 = do
  printTestSolutions 16 $ MkAoCSolution parseInput part1
  --printSolutions 16 $ MkAoCSolution parseInput part2

data ValveData
  = MkValveData
      { _flowRate :: !Integer
      , _children :: ![String]
      }
  deriving (Eq, Generic, Ord, Show)

instance Hashable ValveData

type ValveSystem = HM.HashMap String ValveData

data ValveState
  = MkValveState
      { _currentValve  :: !String
      , _currentTime   :: !Integer
      , _currentFlow   :: !Integer
      , _totalReleased :: !Integer
      , _valvesOn      :: !(HashSet String)
      }
  deriving (Eq, Generic, Ord, Show)

instance Hashable ValveState

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
  pure (name, MkValveData flowrate children)

part1 mp = fmap2 _currentFlow $ solve mp state
  where state = MkValveState "AA" 0 0 0 HS.empty

neighbourNodes :: ValveSystem -> ValveState -> HashSet ValveState
neighbourNodes system state@MkValveState{..} = HS.fromList $! travelValves ++ openCurrenValve --Maybe can make this faster by not concatenating lists
  where cValve@MkValveData{..} = system HM.! _currentValve
        travelValves = map (\child -> state { _currentValve = child,
                                          _currentTime = _currentTime+1,
                                          _totalReleased = _totalReleased + _currentFlow }) _children
        openCurrenValve = let newFlow = _currentFlow + _flowRate
                          in [state { _currentTime = _currentTime+1,
                                    _currentFlow = newFlow,
                                    _totalReleased = _totalReleased + _currentFlow,
                                    _valvesOn = HS.insert _currentValve _valvesOn
                                    } | not (_currentValve `HS.member` _valvesOn)]

--This is still really slow. Maybe I can do this without copying the map of valves everywhere (using the state monad perhaps?)
--Or maybe I can create a better heuristic function?

cost :: ValveSystem -> ValveState -> ValveState -> Integer
cost system _ MkValveState{..} = maxFlow - _currentFlow
  where maxFlow = HM.foldr' (\v a -> _flowRate v + a) 0 system

--Maybe calculate max flow up front?
heuristic :: ValveSystem -> ValveState -> Integer
heuristic system MkValveState{..} = maxFlow - _currentFlow
  where maxFlow = HM.foldr' (\v a -> _flowRate v + a) 0 system

finished :: ValveState -> Bool
finished MkValveState{..} = _currentTime == 30

solve :: ValveSystem -> ValveState -> Maybe [ValveState]
solve system = aStar (neighbourNodes system) (cost system) (heuristic system) (finished)


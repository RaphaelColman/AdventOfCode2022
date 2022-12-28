{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
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
import           Text.Printf             (printf)
import           Text.Trifecta           (CharParsing (anyChar, char, string),
                                          Parser, TokenParsing (token),
                                          commaSep, integer, letter, space,
                                          whiteSpace)

aoc16 :: IO ()
aoc16 = do
  printSolutions 16 $ MkAoCSolution parseInput part1
  --printSolutions 16 $ MkAoCSolution parseInput part2

data ValveData
  = MkValveData
      { _flowRate :: !Integer
      , _children :: ![String]
      }
  deriving (Eq, Generic, Ord, Show)

instance Hashable ValveData

data ValveSystem
  = MkValveSystem
      { _system  :: !(HM.HashMap String ValveData)
      , _maxFlow :: !Integer
      }
  deriving (Eq, Show)

data ValveState
  = MkValveState
      { _currentValve  :: !String
      , _currentTime   :: !Integer
      , _currentFlow   :: !Integer
      , _totalReleased :: !Integer
      , _valvesOn      :: !(HashSet String)
      , _visited       :: !(HashSet String) --There is no point going to a visited node unless we've just turned on a valve, so I'll clear this whenever we turn on a valve
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

part1 mp = fmap2 _totalReleased $ solve system state
  where state = MkValveState "AA" 0 0 0 HS.empty HS.empty
        system = MkValveSystem mp maxFlow
        maxFlow = HM.foldr' (\v a -> _flowRate v + a) 0 mp

--on my input, there are only 15 valves which release pressure.
--max flow is 201

neighbourNodes :: ValveSystem -> ValveState -> HashSet ValveState
neighbourNodes MkValveSystem{..} state@MkValveState{..} = traceShow dbg2 neighbours
  where cValve@MkValveData{..} = _system HM.! _currentValve
        incremented = state { _currentTime = _currentTime+1, _totalReleased = _totalReleased + _currentFlow, _visited = HS.insert _currentValve _visited }
        validChildren = filter (\child -> not (child `HS.member` _visited)) _children
        travelValves = map (\child -> incremented { _currentValve = child }) validChildren
        openCurrentValve = let newFlow = _currentFlow + _flowRate
                           in [incremented {
                                     _currentFlow = newFlow,
                                     _valvesOn = HS.insert _currentValve _valvesOn,
                                     _visited = HS.empty --Clear this down because we can visit old nodes again
                                     } | not (_currentValve `HS.member` _valvesOn)]
        neighbours = if _currentFlow == _maxFlow
              then HS.singleton incremented --no point going anywhere
              else HS.fromList $! travelValves ++ openCurrentValve
        dbg :: String = printf "neighbours: %d, current valve: %s, visited %s, validChildren %s" (length neighbours) (show _currentValve) (show _visited) (show validChildren)
        dbg2 :: String = printf "time: %d, current flow: %s" _currentTime (show _currentFlow)

--This is still really slow. Maybe I can do this without copying the map of valves everywhere (using the state monad perhaps?)
--Or maybe I can create a better heuristic function?

cost :: ValveSystem -> ValveState -> ValveState -> Integer
cost MkValveSystem{..} _ MkValveState{..} = _maxFlow - _currentFlow

--Maybe calculate max flow up front?
heuristic :: ValveSystem -> ValveState -> Integer
heuristic MkValveSystem{..} MkValveState{..} = _maxFlow - _currentFlow

finished :: ValveState -> Bool
finished MkValveState{..} = _currentTime == 30

solve :: ValveSystem -> ValveState -> Maybe [ValveState]
solve system = aStar (neighbourNodes system) (cost system) (heuristic system) (finished)

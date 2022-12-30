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
import           Data.Foldable           (Foldable (toList))
import           Data.Function           ((&))
import           Data.Functor            (($>))
import           Data.Graph.AStar        (aStar)
import qualified Data.HashMap.Strict     as HM
import           Data.HashSet            (HashSet)
import qualified Data.HashSet            as HS
import           Data.Hashable           (Hashable)
import           Debug.Trace             (traceShow)
import           GHC.Generics            (Generic)
import           GHC.OldList             (sort, tails)
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
      , _children :: ![String] --Can make this a set
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

part1 mp = solve mp state
  where state = MkValveState "AA" 0 0 0 HS.empty HS.empty
        maxFlow = HM.foldr' (\v a -> _flowRate v + a) 0 mp

--on my input, there are only 15 valves which release pressure.
--max flow is 201

neighbourNodes :: ValveSystem -> ValveState -> HashSet ValveState
neighbourNodes system state@MkValveState{..} = traceShow dbg neighbours
  where cValve@MkValveData{..} = system HM.! _currentValve
        incremented = state { _currentTime = _currentTime+1, _totalReleased = _totalReleased + _currentFlow, _visited = HS.insert _currentValve _visited }
        validChildren = filter (\child -> not (child `HS.member` _visited)) _children
        travelValves = map (\child -> incremented { _currentValve = child }) validChildren
        openCurrentValve = let newFlow = _currentFlow + _flowRate
                           in [incremented {
                                     _currentFlow = newFlow,
                                     _valvesOn = HS.insert _currentValve _valvesOn,
                                     _visited = HS.singleton _currentValve --Clear this down because we can visit old nodes again
                                     } | not (_currentValve `HS.member` _valvesOn)]
        neighbours = if allValvesOn
              then HS.singleton incremented --no point going anywhere
              else HS.fromList $! travelValves ++ openCurrentValve
        allValvesOn = null $ HM.filterWithKey (\k MkValveData{..} -> not (k `HS.member` _valvesOn) && _flowRate /= 0) system
        dbg :: String = printf "currentFlow: %d, currentTime: %d" _currentFlow _currentTime

--This is still really slow. Maybe I can do this without copying the map of valves everywhere (using the state monad perhaps?)
--Or maybe I can create a better heuristic function?

--Instead of of this, could I just make the cost the sum of the unopened valves?
cost :: ValveSystem -> ValveState -> ValveState -> Integer
cost system _ MkValveState{..} = remainingValvePressure
  where remainingValvePressure = HM.filterWithKey (\k v -> not (k `HS.member` _valvesOn)) system
                          & toList
                          & map _flowRate
                          & sum

--Is there a better heuristic than this? Something like:
--Assume that on every step, I will hit the remaining unopened valves?
heuristic :: ValveSystem -> ValveState -> Integer
heuristic system MkValveState{..} = estimatedCost
  where remainingValves = HM.filterWithKey (\k v -> not (k `HS.member` _valvesOn)) system
                          & toList
                          & map _flowRate
                          & sort
                          & reverse
        remainingPressure = sum remainingValves
        remainingTime = (30 - _currentTime) `div` 2 --It takes two moves for a valve to contribute
        estimatedCost = sum $ map sum $ take (fromInteger remainingTime) $ tails remainingValves

finished :: ValveSystem -> ValveState -> Bool
finished system MkValveState{..} = _currentTime == 30 || allValvesOn 
  where allValvesOn = null $ HM.filterWithKey (\k MkValveData{..} -> not (k `HS.member` _valvesOn) && _flowRate /= 0) system --Maybe this would be easier if I just calculated maxPressure up front

solve :: ValveSystem -> ValveState -> Maybe Integer
solve system state = do
  solved <- aStar (neighbourNodes system) (cost system) (heuristic system) (finished system) state
  let finalState@MkValveState{..} = last solved
  let remainingPressure = (30 - _currentTime) * _currentFlow
  pure $ traceShow finalState $ _totalReleased + remainingPressure

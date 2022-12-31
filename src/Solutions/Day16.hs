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
import           Control.Monad.Reader    (MonadReader (ask), Reader, runReader)
import           Data.Foldable           (Foldable (toList), foldrM)
import           Data.Function           ((&))
import           Data.Functor            (($>))
import           Data.Graph.AStar        (aStar, aStarM)
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
      , _valvesOn      :: !(HashSet (Integer, String)) --(Time left when turned on, label of valve)
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
  where state = MkValveState "AA" 29 0 HS.empty HS.empty
        maxFlow = HM.foldr' (\v a -> _flowRate v + a) 0 mp

--on my input, there are only 15 valves which release pressure.
--max flow is 201

neighbourNodes :: ValveState -> Reader ValveSystem (HashSet ValveState)
neighbourNodes state@MkValveState{..} = do
  system <- ask
  let cValve@MkValveData{..} = system HM.! _currentValve
  let incremented = state { _currentTime = _currentTime-1, _visited = HS.insert _currentValve _visited }
  let validChildren = filter (\child -> not (child `HS.member` _visited)) _children
  let travelValves = map (\child -> incremented { _currentValve = child }) validChildren
  let shouldOpenValve = not (_currentValve `HS.member` valvesOnSet) && _flowRate > 0
  let openCurrentValve = let newFlow = _currentFlow + _flowRate
                     in [incremented {
                               _currentFlow = newFlow,
                               _valvesOn = HS.insert (_currentTime, _currentValve) _valvesOn,
                               _visited = HS.singleton _currentValve --Clear this down because we can visit old nodes again
                               } | shouldOpenValve]
  let allValvesOn = null $ HM.filterWithKey (\k MkValveData{..} -> not (k `HS.member` valvesOnSet) && _flowRate /= 0) system
  let neighbours = if allValvesOn
        then HS.singleton incremented --no point going anywhere
        else HS.fromList $! travelValves ++ openCurrentValve
  let dbg :: String = printf "state: %s, neighbours: %s" (show state) (show (HS.map cv neighbours))
  pure neighbours
  where valvesOnSet = HS.map snd _valvesOn

--This is still really slow. Maybe I can do this without copying the map of valves everywhere (using the state monad perhaps?)
--Or maybe I can create a better heuristic function?

--Instead of of this, could I just make the cost the sum of the unopened valves?
cost :: ValveState -> ValveState -> Reader ValveSystem Integer
cost _ state@MkValveState{..} = do
  system <- ask
  pure $ HM.filterWithKey (\k v -> not (k `HS.member` valvesOnSet state)) system
                          & toList
                          & map _flowRate
                          & sum

--Is there a better heuristic than this? Something like:
--Assume that on every step, I will hit the remaining unopened valves?
heuristic :: ValveState -> Reader ValveSystem Integer
heuristic state@MkValveState{..} = do
  system <- ask
  --let remainingValves = HM.filterWithKey (\k v -> not (k `HS.member` _valvesOn)) system
  --                        & toList
  --                        & map _flowRate
  --                        & sort
  --                        & reverse
  --let remainingTime = (30 - _currentTime) `div` 2 --It takes two moves for a valve to contribute
  --let estimate = sum $ map sum $ take (fromInteger remainingTime) $ tails remainingValves
  --let dbg :: String = printf "state: %s, estimate: %s" (show state) (show estimate)
  --pure estimate
  pure $ HM.filterWithKey (\k v -> not (k `HS.member` valvesOnSet state)) system
                          & toList
                          & map _flowRate
                          & sum

finished :: ValveState -> Reader ValveSystem Bool
finished state@MkValveState{..} = do
  system <- ask
  let allValvesOn = null $ HM.filterWithKey (\k MkValveData{..} -> not (k `HS.member` valvesOnSet state) && _flowRate /= 0) system --Maybe this would be easier if I just calculated maxPressure up front
  pure $ _currentTime == 0 || allValvesOn

solve :: ValveSystem -> ValveState -> Maybe Integer
solve system state = do
  solved <- flip runReader system $ aStarM neighbourNodes cost heuristic finished (pure state)
  let finalState@MkValveState{..} = last solved
  let totalPressure = flip runReader system $ totalPressureReleased finalState --This is a good case for monad transformer so I don't need to runReader twice
  let route = fmap cv solved
  let dbg :: String = printf "route: %s, finalState = %s" (show route) (show finalState)
  traceShow dbg $ pure totalPressure

cv = _currentValve

valvesOnSet :: ValveState -> HashSet String
valvesOnSet MkValveState{..} = HS.map snd _valvesOn

totalPressureReleased :: ValveState -> Reader ValveSystem Integer
totalPressureReleased MkValveState{..} = foldrM go 0 _valvesOn
  where go :: (Integer, String) -> Integer -> Reader ValveSystem Integer
        go (timeRemaining, valve) acc = do
          system <- ask
          let MkValveData flowRate _ = system HM.! valve
          pure $ acc + flowRate * timeRemaining

solve2 :: ValveSystem -> ValveState -> Maybe Integer
solve2 system state = do
  pure undefined

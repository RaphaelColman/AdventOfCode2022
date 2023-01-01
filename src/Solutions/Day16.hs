{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Solutions.Day16
    ( aoc16
    ) where

import           Common.AoCSolutions        (AoCSolution (MkAoCSolution),
                                             printSolutions, printTestSolutions)
import           Common.FunctorUtils        (fmap2)
import           Common.HashSetUtils        (hsFilterM)
import           Common.ListUtils           (freqs)
import           Control.Applicative        (many, some, (<|>))
import           Control.Lens               (makeLenses)
import           Control.Lens.Getter        ((^.))
import           Control.Monad.Cont         (MonadTrans (lift))
import           Control.Monad.Reader       (MonadReader (ask), Reader,
                                             ReaderT (ReaderT, runReaderT),
                                             runReader)
import           Control.Monad.Reader.Class (asks)
import           Control.Monad.Trans.Maybe  (MaybeT (MaybeT, runMaybeT))
import           Data.Foldable              (Foldable (toList), find, foldrM,
                                             maximumBy)
import           Data.Function              (on, (&))
import           Data.Functor               (($>))
import           Data.Graph.AStar           (aStar, aStarM)
import qualified Data.HashMap.Strict        as HM
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HS
import           Data.Hashable              (Hashable)
import           Data.Ord                   (comparing)
import           Debug.Trace                (traceShow, trace)
import           GHC.Generics               (Generic)
import           GHC.OldList                (sort, tails)
import           Text.Parser.Combinators    (Parsing (try))
import           Text.Printf                (printf)
import           Text.Trifecta              (CharParsing (anyChar, char, string),
                                             Parser, TokenParsing (token),
                                             commaSep, integer, letter, space,
                                             whiteSpace)
import Data.Maybe (fromMaybe)
import Data.HashMap.Internal.Strict (alter)
import Safe (maximumByMay)

data ValveData
  = MkValveData
      { _flowRate :: !Integer
      , _children :: ![String] --Can make this a set
      , _label    :: !String
      }
  deriving (Eq, Generic, Ord, Show)

instance Hashable ValveData

type ValveSystem = HM.HashMap String ValveData

data ValveState
  = MkValveState
      { _currentValve :: !String
      , _currentTime  :: !Integer
      , _currentFlow  :: !Integer
      , _valvesOn     :: !(HashSet (Integer, ValveData)) --(Time left when turned on, label of valve)
      , _visited      :: !(HashSet String) --There is no point going to a visited node unless we've just turned on a valve, so I'll clear this whenever we turn on a valve
      }
  deriving (Eq, Generic, Ord, Show)

instance Hashable ValveState

makeLenses ''ValveData
makeLenses ''ValveState

aoc16 :: IO ()
aoc16 = do
  printSolutions 16 $ MkAoCSolution parseInput part1
  --printSolutions 16 $ MkAoCSolution parseInput part2

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
  pure (name, MkValveData flowrate children name)

part1 mp = doSolve mp state
  where state = MkValveState "AA" 29 0 HS.empty HS.empty

neighbourNodes :: ValveState -> Reader ValveSystem (HashSet ValveState)
neighbourNodes state@MkValveState{..} = do
  cValve@MkValveData{..} <- asks (HM.! _currentValve)
  let incremented = state { _currentTime = _currentTime-1, _visited = HS.insert _currentValve _visited }
  let validChildren = filter (\child -> not (child `HS.member` _visited)) _children
  let travelValves = map (\child -> incremented { _currentValve = child }) validChildren
  let shouldOpenValve = not (_currentValve `HS.member` valvesOnSet state) && _flowRate > 0
  let openCurrentValve = let newFlow = _currentFlow + _flowRate
                     in [incremented {
                               _currentFlow = newFlow,
                               _valvesOn = HS.insert (_currentTime, cValve) _valvesOn,
                               _visited = HS.singleton _currentValve --Clear this down because we can visit old nodes again
                               } | shouldOpenValve]
  allValvesOn <- null <$> remainingUsefulValves state
  pure $ if allValvesOn
        then HS.singleton incremented --there is no point going anywhere
        else HS.fromList $! travelValves ++ openCurrentValve

neighbourNodesWithEstimate :: Integer -> ValveState -> Reader ValveSystem (HashSet ValveState)
neighbourNodesWithEstimate estimate state = do
  allNeighbours <- neighbourNodes state
  hsFilterM (canSurpassTotalFlow estimate) allNeighbours

--If you can turn on the current valve then do. Otherwise, if there is a valve with pressure next ot you then just prefer that
naiveNeigbourNodes :: ValveState -> Reader ValveSystem (HashSet ValveState)
naiveNeigbourNodes state = do
  allNeighbours <- neighbourNodes state
  let sameNode = find (\s -> s ^. currentValve == state ^. currentValve) allNeighbours --If it's the same node then we're turning a valve on
  withPressures <- traverse (\state' -> do
                                valveData <- asks (HM.! (state' ^. currentValve))
                                pure (valveData ^. flowRate, state')
                            ) $ toList allNeighbours
  let maybeBest = maximumByMay (compare `on` fst) withPressures
  case maybeBest of
        Nothing -> pure $ maybe allNeighbours HS.singleton sameNode
        Just (bestFlow, state') -> let df = if bestFlow == 0 then allNeighbours else HS.singleton state'
                                    in pure $ maybe df HS.singleton sameNode

canSurpassTotalFlow :: Integer -> ValveState -> Reader ValveSystem Bool
canSurpassTotalFlow max state@MkValveState{..} = do
  remaining <- reverse . sort <$> remainingUsefulValves state
  let otherValves = HS.fromList $ zip [_currentTime, _currentTime-2 .. 0] remaining
  let optimisticValvesOn = HS.union _valvesOn otherValves
  pure $ totalPressureReleased optimisticValvesOn > max

cost :: ValveState -> ValveState -> Reader ValveSystem Integer
cost _ state@MkValveState{..} = do
  remaining <- remainingUsefulValves state
  pure $ sum $ map _flowRate remaining

--It takes two steps to open a valve. So assume I open each remaining valve in descending order every two steps
heuristic :: ValveState -> Reader ValveSystem Integer
heuristic state@MkValveState{..} = do
  remaining <- remainingUsefulValves state
  let sortedValveFlows = reverse . sort $ map _flowRate remaining
  let stepsRemaining = _currentTime `div` 2
  pure $ sum $ map sum $ take (fromInteger stepsRemaining) $ tails sortedValveFlows

finished :: ValveState -> Reader ValveSystem Bool
finished state@MkValveState{..} = do
  remaining <- remainingUsefulValves state
  pure $ _currentTime == 0 || null remaining

valvesOnSet :: ValveState -> HashSet String
valvesOnSet MkValveState{..} = HS.map (_label . snd) _valvesOn

remainingUsefulValves :: ValveState -> Reader ValveSystem [ValveData]
remainingUsefulValves state = do
  system <- ask
  pure $ HM.filterWithKey (\k (MkValveData flowRate _ _) -> flowRate > 0 && not (k `HS.member` valvesOnSet state)) system
          & toList

totalPressureReleased :: HashSet (Integer, ValveData) -> Integer
totalPressureReleased = foldr go 0
  where go (time, MkValveData flowRate _ _) acc =  acc + flowRate * time

doSolve :: ValveSystem -> ValveState -> Maybe Integer
doSolve system state = runReader (runMaybeT (solve state)) system

solve :: ValveState -> MaybeT (Reader ValveSystem) Integer
solve state = do
  naivePath <- MaybeT $ aStarM naiveNeigbourNodes cost heuristic finished (pure state)
  let estimate = totalPressureReleased $ last naivePath ^. valvesOn
  let dbg :: String = printf "The best value must be higher than: %d" estimate
  path <- trace dbg $ MaybeT $ aStarM (neighbourNodesWithEstimate estimate) cost heuristic finished (pure state)
  pure $ totalPressureReleased $ last path ^. valvesOn


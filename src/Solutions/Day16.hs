{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Solutions.Day16
    ( aoc16
    ) where

import           Common.AoCSolutions          (AoCSolution (MkAoCSolution),
                                               printSolutions,
                                               printTestSolutions)
import           Common.FunctorUtils          (fmap2)
import           Common.HashSetUtils          (hsFilterM, hsNotMember)
import           Common.ListUtils             (freqs)
import           Control.Applicative          (many, some, (<|>))
import           Control.Lens                 (At (at), ix, makeLenses, mapped,
                                               over, set)
import           Control.Lens.Combinators     (Lens')
import           Control.Lens.Getter          (view, (^.))
import           Control.Lens.Operators       ((^?))
import           Control.Lens.Setter          ((.~))
import           Control.Monad.Cont           (MonadTrans (lift))
import           Control.Monad.Reader         (MonadReader (ask), Reader,
                                               ReaderT (ReaderT, runReaderT),
                                               runReader)
import           Control.Monad.Reader.Class   (asks)
import           Control.Monad.Trans.Maybe    (MaybeT (MaybeT, runMaybeT))
import           Data.Foldable                (Foldable (toList), find, foldlM,
                                               foldrM, maximumBy)
import           Data.Function                (on, (&))
import           Data.Functor                 (($>))
import           Data.Graph.AStar             (aStar, aStarM)
import           Data.HashMap.Internal.Strict (alter)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HM
import           Data.HashSet                 (HashSet)
import qualified Data.HashSet                 as HS
import           Data.Hashable                (Hashable)
import           Data.List                    (sortOn)
import           Data.Maybe                   (fromMaybe)
import           Data.Ord                     (Down (Down), comparing)
import           Debug.Trace                  (trace, traceShow)
import           GHC.Base                     (Any)
import           GHC.Generics                 (Generic)
import           GHC.OldList                  (sort, tails)
import           Safe                         (headMay)
import           Safe.Foldable                (maximumByMay, maximumMay)
import           Text.Parser.Combinators      (Parsing (try))
import           Text.Printf                  (printf)
import           Text.Trifecta                (CharParsing (anyChar, char, string),
                                               Parser, TokenParsing (token),
                                               commaSep, integer, letter, space,
                                               whiteSpace)

data ValveData
  = MkValveData
      { _flowRate :: !Integer
      , _children :: ![String] --Can make this a set
      , _label    :: !String
      }
  deriving (Eq, Generic, Ord, Show)

instance Hashable ValveData

type ValveSystem = HashMap String ValveData

data Agent
  = MkAgent
      { _currentValve :: !String
      , _visited      :: !(HashSet String)
      }
  deriving (Eq, Generic, Ord, Show)

instance Hashable Agent

data ValveState
  = MkValveState
      { _agents      :: !(HashMap Integer Agent)
      , _currentTime :: !Integer
      , _currentFlow :: !Integer
      , _valvesOn    :: !(HashSet (Integer, ValveData)) --(Time left when turned on, label of valve)
      }
  deriving (Eq, Generic, Ord, Show)

instance Hashable ValveState

makeLenses ''ValveData
makeLenses ''ValveState
makeLenses ''Agent

aoc16 :: IO ()
aoc16 = do
  --printSolutions 16 $ MkAoCSolution parseInput part1
  printSolutions 16 $ MkAoCSolution parseInput part2

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
  where state = MkValveState agents 29 0 HS.empty
        agents = HM.fromList [(0, MkAgent "AA" HS.empty)]

part2 mp = doSolve mp state
  where state = MkValveState agents 25 0 HS.empty
        agents = MkAgent "AA" HS.empty
                & repeat
                & zip [0,1]
                & HM.fromList

--You can get into a stage where one agent is stuck because the only child valve they can visit is already in their
--visited set and it has been turned on by the other agent. They will never turn it on (which would reset their visited set).
neighbourNodesForAgent :: Integer -> ValveState -> Reader ValveSystem (HashSet ValveState)
neighbourNodesForAgent index state@MkValveState{..} = do
  let agent@MkAgent{..} = (state ^. agents) HM.! index
  cValve@MkValveData{..} <- asks (HM.! _currentValve)
  let validChildren = filter (`hsNotMember` _visited) _children
  let travelValves = map (\dest -> set (agents . ix index) (agent { _currentValve = dest, _visited = HS.insert _currentValve _visited}) state) validChildren
  let shouldOpenValve = not (_currentValve `HS.member` valvesOnSet state) && _flowRate > 0
  let openCurrentValve = [state
          & valvesOn .~ HS.insert (_currentTime, cValve) _valvesOn
          & (agents . ix index) . visited .~ HS.singleton _currentValve
          & currentFlow .~ (_currentFlow + _flowRate)
          | shouldOpenValve]
  let dbg :: String = printf "agent %d: original: %s, newstates: %s" index (show state) (show (travelValves ++ openCurrentValve))
  --pure $ HS.fromList $  travelValves ++ openCurrentValve
  --This is for the case where one agent goes passed a valve and the other agent turns the valve on (the first agent will be stuck). I don't think I need it
  let newStates = travelValves ++ openCurrentValve
  pure $ if null newStates
          then HS.singleton $ state & (agents . ix index) .visited .~ HS.singleton _currentValve
          else HS.fromList newStates

neighbourNodes :: ValveState -> Reader ValveSystem (HashSet ValveState)
neighbourNodes state@MkValveState{..} = do
  let agentIndexes = HM.keys (state ^. agents)
  states <- foldlM go (HS.singleton state) agentIndexes
  pure $ HS.map (\s -> s { _currentTime = _currentTime - 1 }) states
  where go :: HashSet ValveState -> Integer -> Reader ValveSystem (HashSet ValveState)
        go states agentIndex = do
          newStates <- traverse (neighbourNodesForAgent agentIndex) $ toList states
          pure $ HS.unions newStates

getCurrentValves vs = vs ^. agents & HM.map _currentValve

neighbourNodesWithEstimate :: Integer -> ValveState -> Reader ValveSystem (HashSet ValveState)
neighbourNodesWithEstimate estimate state = do
  allNeighbours <- neighbourNodes state
  filteredStates <- hsFilterM (canSurpassTotalFlow estimate) allNeighbours
  let currentTime = fmap _currentTime $ headMay $ toList filteredStates
  let dbg :: String = printf "before filter: %d, after filter: %d" (length allNeighbours) (length filteredStates)
  let filterHelped :: String = printf "filter helped: %s"  (show (length allNeighbours > length filteredStates))
  pure filteredStates

--This needs to to take into account that there are multiple agents
canSurpassTotalFlow :: Integer -> ValveState -> Reader ValveSystem Bool
canSurpassTotalFlow max state@MkValveState{..} = do
  remaining <- reverse . sort <$> remainingUsefulValves state
  let numberOfAgents = length $ state ^. agents
  let countdown = concatMap (replicate numberOfAgents) [_currentTime, _currentTime-2 .. 0]
  let otherValves = HS.fromList $ zip countdown remaining
  let optimisticValvesOn = HS.union _valvesOn otherValves
  let dbg :: String = printf "optimistic valves on: %s, optimistic total: %d, max estimate: %d" (show optimisticValvesOn) (totalPressureReleased optimisticValvesOn) max
  pure $ totalPressureReleased optimisticValvesOn >= max

cost :: ValveState -> ValveState -> Reader ValveSystem Integer
cost _ state@MkValveState{..} = do
  remaining <- remainingUsefulValves state
  pure $ sum $ map _flowRate remaining

--It takes two steps to open a valve. So assume I open each remaining valve in descending order every two steps
--This needs to take into account that there are multiple agents
heuristic :: ValveState -> Reader ValveSystem Integer
heuristic state@MkValveState{..} = do
  remaining <- remainingUsefulValves state
  let sortedValveFlows = reverse . sort $ map _flowRate remaining
  let stepsRemaining = _currentTime `div` 2
  --pure $ sum $ map sum $ take (fromInteger stepsRemaining) $ tails sortedValveFlows
  pure $ sum $ map _flowRate remaining


finished :: ValveState -> Reader ValveSystem Bool
finished state@MkValveState{..} = do
  remaining <- remainingUsefulValves state
  pure $ _currentTime <= 0 || null remaining

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
  naiveEstimate <- lift $ naiveSearch 0 25 (HS.singleton state) (reduceStatesNaively 100)
  let dbg :: String = printf "The best value must be higher than or equal to: %d" naiveEstimate
  --path <- trace dbg $ MaybeT $ aStarM (neighbourNodesWithEstimate naiveEstimate) cost heuristic finished (pure state)
  lift $ naiveSearch 0 25 (HS.singleton state) $ reduceStatesNaively 1000
  --let finalState = last path
  --let dbg2 :: String = printf "valvesOn at end: %s" (show (finalState ^. valvesOn))
  --trace dbg2 $ pure $ totalPressureReleased $ finalState ^. valvesOn


--No finish state yet!
naiveSearch :: Integer -> Integer -> HashSet ValveState -> Reducer -> Reader ValveSystem Integer
naiveSearch bestScore timeRemaining states reducer = do
  nextStates <- HS.unions <$> traverse neighbourNodes (toList states)
  let bestState = maximumMay $ HS.map (totalPressureReleased . _valvesOn) nextStates
  let newBestScore = max bestScore (fromMaybe 0 bestState)
  reducedStates <- reducer nextStates
  let dbg :: String = printf "states: %d, reducedStates: %d" (length nextStates) (length reducedStates)
  if timeRemaining /= 0 || null states
  then naiveSearch newBestScore (timeRemaining-1) reducedStates reducer
  else pure newBestScore

type Reducer = HashSet ValveState -> Reader ValveSystem (HashSet ValveState)

reduceStatesBasedOnEstimate :: Integer -> HashSet ValveState -> Reader ValveSystem (HashSet ValveState)
reduceStatesBasedOnEstimate estimate = hsFilterM (canSurpassTotalFlow estimate)

reduceStatesNaively :: Integer -> HashSet ValveState -> Reader ValveSystem (HashSet ValveState)
reduceStatesNaively amount states = do
  let reducedStates = take (fromInteger amount) $ sortOn (Down . (totalPressureReleased . _valvesOn)) (toList states)
  pure $ HS.fromList reducedStates

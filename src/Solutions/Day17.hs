{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Solutions.Day17 where

import           Common.AoCSolutions      (AoCSolution (MkAoCSolution),
                                           printSolutions, printTestSolutions)
import           Common.Geometry          (enumerateMultilineStringToVectorMap,
                                           renderVectorSet)
import           Control.Applicative      ((<|>))
import           Control.Lens             (makeLenses, set)
import           Control.Lens.Getter      ((^.))
import           Control.Lens.Setter      (over, (.~))
import           Control.Monad.Extra      (iterateM)
import           Control.Monad.Loops      (iterateM_, untilM)
import           Control.Monad.State      (MonadState (put), State,
                                           StateT (runStateT), get, gets,
                                           runState)
import           Control.Monad.State.Lazy (state)
import           Data.Foldable            (Foldable (foldl'), foldlM, minimumBy)
import           Data.Function            (on, (&))
import           Data.List                (intersperse)
import qualified Data.Map                 as M
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Debug.Trace              (trace, traceShow)
import           GHC.OldList              (scanl')
import           Linear                   (V2 (V2))
import           Linear.V2                (R1 (_x), R2 (_y), V2)
import           Linear.Vector            (unit)
import           Text.Parser.Char         (CharParsing (char))
import           Text.Parser.Combinators  (Parsing (try), some)
import           Text.Printf              (printf)
import           Text.RawString.QQ        (r)
import           Text.Trifecta            (CharParsing (anyChar), Parser)

data Direction = LEFT | RIGHT deriving (Enum, Eq, Ord, Show)
data Action
  = DROP
  | WIND !Direction
  deriving (Eq, Ord, Show)
type Rock = Set (V2 Int)
data CaveState
  = MkState
      { _currentRock :: !Rock
      , _grid        :: !(Set (V2 Int))
      , _rockList    :: [Rock]
      , _actionList  :: [Action]
      , _rockCount   :: !Integer
      }
  deriving (Eq)

makeLenses ''CaveState

instance Show CaveState where
  show :: CaveState -> String
  show cs = let fullGrid = (cs ^. grid) `S.union` (cs ^. currentRock)
            in renderVectorSet fullGrid


aoc17 :: IO ()
aoc17 = do
  printTestSolutions 17 $ MkAoCSolution parseInput part1
  --printSolutions 17 $ MkAoCSolution parseInput part2

parseInput :: Parser [Direction]
parseInput = some parseDirection
  where parseDirection :: Parser Direction
        parseDirection = do
          c <- try (char '<') <|> try (char '>')
          case c of
            '<' -> pure LEFT
            '>' -> pure RIGHT
            _   -> fail $ printf "Unrecognised character %s" c

part1 = solve

solve :: [Direction] -> V2 Int
solve directions = minimumBy (compare `on` (^. _y)) $ cs ^. grid
  where (a, cs) = flip runState (initState directions) $ do
                stepState `untilM` do gets ((==3) . (^. rockCount))


initState :: [Direction] -> CaveState
initState directions = MkState initialRock initialGrid rockList actions 0
  where initialGrid = S.fromList $ map (`V2` 0) [0..6] --Add a floor
        initialRock = S.map (+ V2 2 (-4)) $ head rocks
        actions = initActions directions
        rockList = tail $ cycle rocks

--I can't just dumbly alternate between wind and drop actions because every time a rock is spawned
--the first action needs to be wind
initActions :: [Direction] -> [Action]
initActions = cycle . intersperse DROP . map WIND

stepState :: State CaveState CaveState
stepState = do
  st <- get
  act <- nextAction
  moveRock act
  result <- get
  let dbg :: String = printf "action: %s" (show act)
  traceShow (result, dbg) $ pure result
  where moveRock action = case action of
          DROP           -> dropRock
          WIND direction -> blowRock direction

nextRock :: State CaveState Rock
nextRock = state $ \st -> (st ^. rockList & head, st & over rockList tail)

nextAction :: State CaveState Action
nextAction = state $ \st -> (st ^. actionList & head, st & over actionList tail)

dropRock :: State CaveState ()
dropRock = do
  cs <- get
  let newPos = S.map (\v -> v + unit _y) (cs ^. currentRock)
  if overlaps newPos (cs ^. grid)
  then spawnNewRock
  else put $ cs & currentRock .~ newPos

--This is not spawning the L-shaped rock in the right place (it's too close to the bottom)
--Topmost is wrong. It should be calculated AFTER we've merged the current rock
spawnNewRock :: State CaveState ()
spawnNewRock = do
  next <- nextRock
  cs <- get
  let tm = topMost (cs ^. currentRock)
  let bm = bottomMost next
  let yDiff = topMost (cs ^. currentRock) - 4 - bottomMost next
  let newCurrent = S.map (+ V2 2 yDiff) next
  let newCs = cs & over grid (`S.union` (cs ^. currentRock)) & currentRock .~ newCurrent  & over rockCount (+1)
  let dbg :: String = printf "next rock %s, topMost: %d, bottomMost: %d, yDiff: %d" (show next) tm bm yDiff
  put newCs
topMost grid = minimum $ S.map (^. _y) grid
bottomMost grid = maximum $ S.map (^. _y) grid
leftMost grid = minimum $ S.map (^. _x) grid

--The second rock is not getting blown to the right.
blowRock :: Direction -> State CaveState ()
blowRock direction = do
  cs <- get
  let newPos = case direction of
        LEFT  -> S.map (\v -> v - unit _x) (cs ^. currentRock)
        RIGHT -> S.map (\v -> v + unit _x) (cs ^. currentRock)
  if overlaps newPos (cs ^. grid)
  then put cs
  else put $ cs & currentRock .~ newPos

overlaps :: Rock -> Set (V2 Int) -> Bool
overlaps rock cave = wall || otherRock
  where wall = any (\(V2 x _) -> x <= 0 || x > 6) rock
        otherRock = not $ null $ S.intersection rock cave

rocks :: [Rock]
rocks = S.fromList . M.keys . M.filter (=='#') <$>
        map enumerateMultilineStringToVectorMap ["####",[r|.#.
###
.#.
|], [r|..#
..#
###
|], [r|#
#
#
#
|], [r|##
##
|]]

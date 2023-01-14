{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Solutions.Day17 where

import           Common.AoCSolutions      (AoCSolution (MkAoCSolution),
                                           printSolutions, printTestSolutions)
import           Common.Debugging         (traceLns)
import           Common.Floyd             (hareAndTortoise, index, node, CycleData (MkCycleData))
import qualified Common.Floyd             as Floyd (length)
import           Common.Geometry          (enumerateMultilineStringToVectorMap,
                                           renderVectorSet)
import           Control.Applicative      ((<|>))
import           Control.Lens             (makeLenses, set)
import           Control.Lens.Getter      ((^.))
import           Control.Lens.Setter      (over, (.~))
import           Control.Monad.Extra      (iterateM)
import           Control.Monad.Loops      (iterateM_, untilM)
import           Control.Monad.State      (MonadState (put), State,
                                           StateT (runStateT), execState, get,
                                           gets, runState)
import           Control.Monad.State.Lazy (state)
import           Data.Foldable            (Foldable (foldl'), foldlM, maximumBy,
                                           minimumBy)
import           Data.Function            (on, (&))
import           Data.List                (intersperse)
import qualified Data.Map                 as M
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Debug.Trace              (trace, traceM, traceShow, traceShowM)
import           GHC.OldList              (scanl')
import           Linear                   (V2 (V2))
import           Linear.V2                (R1 (_x), R2 (_y), V2)
import           Linear.Vector            (unit)
import           Text.Parser.Char         (CharParsing (char))
import           Text.Parser.Combinators  (Parsing (try), some)
import           Text.Printf              (printf)
import           Text.RawString.QQ        (r)
import           Text.Trifecta            (CharParsing (anyChar), Parser)
import Data.Sequence (iterateN)

data Direction = LEFT | RIGHT deriving (Enum, Eq, Ord, Show)

type Rock = Set (V2 Int)
data CaveState
  = MkState
      { _currentRock    :: !Rock
      , _grid           :: !(Set (V2 Int))
      , _rockList       :: ![Rock]
      , _directionList  :: ![Direction]
      , _directionPhase :: !Int --Total number of directions before the cycle repeats
      , _rockCount      :: !Integer
      }
  deriving (Eq)

data CaveProfile
  = MkCaveProfile
      { _topLayer   :: !(Set (V2 Int))
      , _cRock      :: !Rock
      , _directions :: ![Direction]
      }
  deriving (Eq, Show)

makeLenses ''CaveState
makeLenses ''CaveProfile

instance Show CaveState where
  show :: CaveState -> String
  show cs = let fullGrid = (cs ^. grid) `S.union` (cs ^. currentRock)
            in renderVectorSet fullGrid


toProfile :: CaveState -> CaveProfile
toProfile cs = MkCaveProfile tops (normalise (cs ^. currentRock)) directions
  where topForX x = minimumBy (compare `on` (^. _y)) $ S.filter (\v -> (v ^. _x) == x) $ cs ^. grid
        tops = normalise $ S.fromList $ map topForX [0..6]
        directions = take (cs ^. directionPhase) (cs ^. directionList)

normalise :: Set (V2 Int) -> Set (V2 Int)
normalise points = S.map (\v -> v - minimum points) points

aoc17 :: IO ()
aoc17 = do
  --printTestSolutions 17 $ MkAoCSolution parseInput part1
  printSolutions 17 $ MkAoCSolution parseInput part2

parseInput :: Parser [Direction]
parseInput = some parseDirection
  where parseDirection :: Parser Direction
        parseDirection = do
          c <- try (char '<') <|> try (char '>')
          case c of
            '<' -> pure LEFT
            '>' -> pure RIGHT
            _   -> fail $ printf "Unrecognised character %s" c


part1 :: [Direction] -> V2 Int
part1 directions = minimumBy (compare `on` (^. _y)) $ cs ^. grid
  where (_, cs) = flip runState (initState directions) $ do
                ffState `untilM` gets ((==2022) . (^. rockCount))

--part2 :: [Direction] -> V2 Int
part2 = calculateHeightFromCycles

--calculateHeightFromCycles :: [Direction] -> Maybe Int
calculateHeightFromCycles directions = do
  let start = initState directions
  (MkCycleData index cycleLength startOfCycleNode) <- hareAndTortoise doRockFall start toProfile
  let [c1, c2] = take 2 $ iterate (applyN cycleLength doRockFall) startOfCycleNode
  let cycleHeight = height c2 - height c1
  let initialHeight = height startOfCycleNode
  let (times, remainder) = (1000000000000 - index) `divMod` cycleLength
  let remainingHeight = height (applyN remainder doRockFall c1) - initialHeight
  pure $ initialHeight + (cycleHeight * times) + remainingHeight
  where doRockFall = execState ffState

--28 = start of cycle
--63 = start of next cycle
--cycle length = 35
--cycle height = 53
--height before start of cycle: 47?
--init + two cycles = 49 + 53 + 53 = 155 (height)
--numRocks for init + two cycles = 28+35+35 = 98

height cs = cs ^. grid
            & S.map (^. _y)
            & minimum
            & abs

applyN :: Int -> (a -> a) -> a -> a
applyN times f x = iterate f x !! times

initState :: [Direction] -> CaveState
initState directions = MkState initialRock initialGrid rockList directionList (length directions) 0
  where initialGrid = S.fromList $ map (`V2` 0) [0..6] --Add a floor
        initialRock = S.map (+ V2 2 (-4)) $ head rocks
        directionList = initActions directions
        rockList = tail $ cycle rocks

--I can't just dumbly alternate between wind and drop actions because every time a rock is spawned
--the first action needs to be wind
initActions :: [Direction] -> [Direction]
initActions = cycle

--Step state until a new rock is spawned
ffState :: State CaveState ()
ffState = do
  numRocks <- gets (^. rockCount)
  stepState `untilM` gets ((/= numRocks) . (^. rockCount))
  pure ()

stepState :: State CaveState ()
stepState = do
  blowRock
  dropRock

nextRock :: State CaveState Rock
nextRock = state $ \st -> (st ^. rockList & head, st & over rockList tail)

nextDirection :: State CaveState Direction
nextDirection = state $ \st -> (st ^. directionList & head, st & over directionList tail)

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
  let newGrid = (cs ^. grid) `S.union` (cs ^. currentRock)
  let yDiff = topMost newGrid - 4 - bottomMost next
  let newCurrent = S.map (+ V2 2 yDiff) next
  let newCs = cs & grid .~ newGrid & currentRock .~ newCurrent & over rockCount (+1)
  put newCs
topMost grid = minimum $ S.map (^. _y) grid
bottomMost grid = maximum $ S.map (^. _y) grid
leftMost grid = minimum $ S.map (^. _x) grid

--The second rock is not getting blown to the right.
blowRock :: State CaveState ()
blowRock = do
  direction <- nextDirection
  --traceM $ printf "Blowing rock %s" (show direction)
  cs <- get
  let newPos = case direction of
        LEFT  -> S.map (\v -> v - unit _x) (cs ^. currentRock)
        RIGHT -> S.map (\v -> v + unit _x) (cs ^. currentRock)
  if overlaps newPos (cs ^. grid)
  then put cs
  else put $ cs & currentRock .~ newPos

overlaps :: Rock -> Set (V2 Int) -> Bool
overlaps rock cave = wall || otherRock
  where wall = any (\(V2 x _) -> x < 0 || x > 6) rock
        otherRock = not $ null $ S.intersection rock cave

rockProfile :: CaveState -> Integer
rockProfile = undefined

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

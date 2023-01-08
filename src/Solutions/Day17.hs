{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Solutions.Day17
    ( aoc17
    ) where

import           Common.AoCSolutions      (AoCSolution (MkAoCSolution),
                                           printSolutions, printTestSolutions)
import           Common.Floyd             (hareAndTortoise, hareAndTortoise)
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
import           Data.Foldable            (Foldable (foldl'), foldlM, minimumBy)
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

data Direction = LEFT | RIGHT deriving (Enum, Eq, Ord, Show)

type Rock = Set (V2 Int)
data CaveState
  = MkState
      { _currentRock   :: !Rock
      , _grid          :: !(Set (V2 Int))
      , _rockList      :: ![Rock]
      , _directionList :: ![Direction]
      , _rockCount     :: !Integer
      }
  deriving (Eq)

data CaveProfile
  = MkCaveProfile
      { _topLayer   :: !(Set (V2 Int))
      , _cRock      :: !Rock
      , _directions :: ![Direction]
      }
  deriving (Eq)

makeLenses ''CaveState

instance Show CaveState where
  show :: CaveState -> String
  show cs = let fullGrid = (cs ^. grid) `S.union` (cs ^. currentRock)
            in renderVectorSet fullGrid


aoc17 :: IO ()
aoc17 = do
  printSolutions 17 $ MkAoCSolution parseInput part1
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


part1 :: [Direction] -> V2 Int
part1 directions = minimumBy (compare `on` (^. _y)) $ cs ^. grid
  where (_, cs) = flip runState (initState directions) $ do
                stepState `untilM` gets ((==2022) . (^. rockCount))

--part2 :: [Direction] -> V2 Int
part2 directions = hareAndTortoise (execState stepState) (initState directions)

initState :: [Direction] -> CaveState
initState directions = MkState initialRock initialGrid rockList actions 0
  where initialGrid = S.fromList $ map (`V2` 0) [0..6] --Add a floor
        initialRock = S.map (+ V2 2 (-4)) $ head rocks
        actions = initActions directions
        rockList = tail $ cycle rocks

--I can't just dumbly alternate between wind and drop actions because every time a rock is spawned
--the first action needs to be wind
initActions :: [Direction] -> [Direction]
initActions = cycle

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

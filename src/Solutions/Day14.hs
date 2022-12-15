{-# LANGUAGE RecordWildCards #-}
module Solutions.Day14 where

import           Common.AoCSolutions   (AoCSolution (MkAoCSolution),
                                        printSolutions, printTestSolutions)
import           Common.Debugging      (traceLns)
import           Common.FunctorUtils   (fmap2)
import           Common.Geometry       (Grid, Point, renderVectorMap,
                                        renderVectorSet)
import           Common.ListUtils      (flexibleRange)
import           Control.Lens.Getter   ((^.))
import           Data.Foldable         (Foldable (foldl'), maximumBy, minimumBy)
import           Data.Function         (on, (&))
import           Data.Functor.Base     (ListF (Cons, Nil))
import           Data.Functor.Foldable (Corecursive (ana))
import           Data.List             (find)
import qualified Data.Map              as M
import           Data.Maybe            (isJust)
import qualified Data.Set              as S
import           Debug.Trace           (traceShow)
import           Linear                (V2 (V2))
import           Linear.V2             (R1 (_x), R2 (_y), V2 (V2))
import           Linear.Vector         (unit)
import           Text.Trifecta         (CharParsing (string), Parser,
                                        Parsing (eof), TokenParsing (token),
                                        comma, commaSep, integer, manyTill,
                                        sepBy, some, whiteSpace)

aoc14 :: IO ()
aoc14 = do
  printTestSolutions 14 $ MkAoCSolution parseInput part1
  printTestSolutions 14 $ MkAoCSolution parseInput part2

type Path = [Point]
type Rocks = S.Set Point

data Object = Rock | Sand deriving (Bounded, Enum, Eq, Show)
data State
  = MkState
      { _grid        :: !(M.Map Point Object)
      , _fallingSand :: !(Maybe Point)
      , _floorHeight :: !(Maybe Int)
      , _steps       :: !Int
      }
  deriving (Eq, Show)


parseInput :: Parser [Path]
parseInput = manyTill parsePath eof
  where parsePoint :: Parser Point
        parsePoint = do
          [x, y] <- fmap2 fromInteger $ commaSep integer
          pure (V2 x y)
        parsePath = sepBy parsePoint (string "->" >> whiteSpace)

part1 :: [Path] -> Int
part1 = solve False

part2 :: [Path] -> Int
part2 = solve True

solve :: Bool -> [Path] -> Int
solve hasFloor paths = let finalState = runSand $ initMap hasFloor paths
                      in length $ M.filter (==Sand) $ _grid finalState

initMap :: Bool -> [Path] -> State
initMap hasFloor paths = MkState grid Nothing floorHeight 0
  where grid = M.fromSet (const Rock) $ S.unions $ map fillPath paths
        floorHeight = let (V2 _ lowest) = maximumBy (compare `on` (^. _y)) (M.keys grid)
                            in if hasFloor then Just (lowest + 2) else Nothing


fillPath :: Path -> Rocks
fillPath [] = S.empty
fillPath (first:rest) = fst $ foldl' (\(rocks, previousPoint) pnt -> let newRocks = S.fromList $ fillIn previousPoint pnt
                                                                              in (S.union newRocks rocks, pnt))
                                                                          (S.empty, first) rest

fillIn :: Point -> Point -> Path
fillIn (V2 x1 y1) (V2 x2 y2)
  | x1 == x2 = [V2 x1 (fromInteger y) | y <- flexibleRange (toInteger y1) (toInteger y2)]
  | y1 == y2 = [V2 (fromInteger x) y1 | x <- flexibleRange (toInteger x1) (toInteger x2)]
  | otherwise = error "Non orthogonal points"

runSand :: State -> State
runSand = last . ana go
  where go :: State -> ListF State State
        go state
            | isFinished state = Nil
            | otherwise = Cons state (step state)

step :: State -> State
step s@MkState{..} = case _fallingSand of
  Nothing -> s { _fallingSand = Just (V2 500 0) }
  Just sand -> case nextSpace of
    Nothing -> s { _grid = M.insert sand Sand _grid, _fallingSand = Nothing, _steps = _steps + 1}
    Just ns -> s { _fallingSand = Just ns, _steps = _steps + 1 }
    where nextSpace = let down = sand + unit _y
                          downLeft = down - unit _x
                          downRight = down + unit _x
                      in find validNextSpace [down, downLeft, downRight]
          hasFloor = isJust _floorHeight
          isFloorHeight (V2 _ y) = Just y == _floorHeight
          isOccupied = flip M.member _grid
          validNextSpace pt = not (isOccupied pt) && not (isFloorHeight pt && hasFloor)

isFinished :: State -> Bool
isFinished s@MkState{..} = if isJust _floorHeight
                          then M.lookup (V2 500 0) _grid == Just Sand
                          else sandCascade s

sandCascade :: State -> Bool
sandCascade MkState{..} = case _fallingSand of
  Nothing       -> False
  Just (V2 x y) -> y > lowestRock ^. _y
  where lowestRock = M.keys _grid
                    & maximumBy (compare `on` (^. _y))

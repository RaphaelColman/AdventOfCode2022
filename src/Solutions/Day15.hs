{-# LANGUAGE RecordWildCards #-}
module Solutions.Day15
    ( aoc15
    ) where

import           Common.AoCSolutions            (AoCSolution (MkAoCSolution),
                                                 printSolutions,
                                                 printTestSolutions)
import           Common.Debugging               (traceLns)
import           Common.FunctorUtils            (fmap2)
import           Common.Geometry                (Point, manhattanDistance,
                                                 renderVectorSet)
import           Common.ListUtils               (flexibleRange)
import           Common.MapUtils                (maximumValue)
import           Control.Lens                   ((^.))
import           Control.Monad                  (filterM)
import           Control.Monad.RWS              (MonadReader (ask))
import           Control.Monad.Reader           (Reader, ReaderT, runReader)
import           Data.Foldable                  (find, maximumBy, minimumBy)
import           Data.Function                  (on)
import           Data.List                      (tails)
import qualified Data.Map                       as M
import           Data.Maybe                     (catMaybes, isJust)
import qualified Data.Set                       as S
import           Debug.Trace                    (traceShow)
import           GHC.OldList                    (inits)
import           Linear                         (V2 (V2))
import           Linear.V2                      (R1 (_x), R2 (_y), V2 (V2),
                                                 perp)
import           Safe                           (headMay)
import           Text.Trifecta                  (CharParsing (string), Parser,
                                                 TokenParsing (token), integer,
                                                 some)
import           Text.Trifecta.Util.IntervalMap (search)

aoc15 :: IO ()
aoc15 = do
  printSolutions 15 $ MkAoCSolution parseInput part1
  printTestSolutions 15 $ MkAoCSolution parseInput part2

data Sensor
  = MkSensor
      { _sensor :: !Point
      , _beacon :: !Point
      }
  deriving (Eq, Show)

parseInput :: Parser [Sensor]
parseInput = some $ token parseSensorBeaconPair
  where parseSensorBeaconPair :: Parser Sensor
        parseSensorBeaconPair = do
          sensorX <- string "Sensor at x=" *> integer <* string ", y="
          sensorY <- integer
          beaconX <- string ": closest beacon is at x=" *> integer <* string ", y="
          beaconY <- integer
          let sensor = fromInteger <$> V2 sensorX sensorY
          let beacon = fromInteger <$> V2 beaconX beaconY
          pure $ MkSensor sensor beacon

part1 :: [Sensor] -> Int
part1 = solve1 2000000

part2 :: [Sensor] -> Maybe Int
part2 = solve2 20

solve1 :: Int -> [Sensor] -> Int
solve1 yVal sensors = length $ S.difference cc beaconsOnRow
  where cc = combinedCrossSections yVal sensors
        beaconsOnRow = S.fromList $ map (^. _x) $ filter (\b -> b ^. _y == yVal) $ map _beacon sensors

solve2 :: Int -> [Sensor] -> Maybe Int
solve2 searchSpace sensors = fmap tuningFrequency $ flip runReader sensors $ do
  validPoints <- catMaybes <$> traverse (validBoundaryPoints searchSpace) sensors
  pure $ headMay validPoints

-- Did some algebra to work this out: if we know the y coordinate, we can find the
-- two points on the rhombus for that y coordinate which have the correct manhattan distance
yCrossSection :: Int -> Sensor -> S.Set Int
yCrossSection yval MkSensor{..} = S.fromList [x1..x2] --This will be empty if the manhattan distance is too small because x1 will be > than x2
  where mdistance = manhattanDistance _sensor _beacon
        x1 = (_sensor ^. _x) - (mdistance - abs (_sensor ^. _y - yval))
        x2 = (_sensor ^. _x) + (mdistance - abs (_sensor ^. _y - yval))

combinedCrossSections :: Int -> [Sensor] -> S.Set Int
combinedCrossSections yVal = S.unions . map (yCrossSection yVal)

--All the points immediately outside the rhombus
validBoundaryPoints :: Int -> Sensor -> Reader [Sensor] (Maybe Point)
validBoundaryPoints range MkSensor{..} = do
  headMay <$> filterM outsideOfSensorRanges allBoundaryPoints
  where mDistance = manhattanDistance _sensor _beacon
        justTopRight = zipWith V2 (flexibleRange 0 (mDistance + 1)) (flexibleRange (mDistance + 1) 0)
        allVectors = concatMap (take 4 . iterate perp) justTopRight
        allBoundaryPoints = filter (inSearchSpace range) $ map (+ _sensor) allVectors

inSearchSpace :: Int -> Point -> Bool
inSearchSpace range (V2 x y) = (x >= 0 && x <= range)
                              && (y >= 0 && y <= range)

outsideOfSensorRanges :: Point -> Reader [Sensor] Bool
outsideOfSensorRanges pt = do
  all (\MkSensor{..} -> manhattanDistance _sensor _beacon < manhattanDistance pt _sensor) <$> ask

tuningFrequency :: Point -> Int
tuningFrequency (V2 x y) = x*4000000 + y

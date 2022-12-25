{-# LANGUAGE RecordWildCards #-}
module Solutions.Day15
    ( aoc15
    ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.Debugging    (traceLns)
import           Common.FunctorUtils (fmap2)
import           Common.Geometry     (Point, manhattanDistance, renderVectorSet)
import           Common.ListUtils    (flexibleRange)
import           Control.Lens        ((^.))
import           Data.Foldable       (maximumBy, minimumBy)
import           Data.Function       (on)
import           Data.List           (tails)
import qualified Data.Set            as S
import           Debug.Trace         (traceShow)
import           GHC.OldList         (inits)
import           Linear              (V2 (V2))
import Linear.V2 ( perp, R1(_x), R2(_y), V2(V2) )
import           Text.Trifecta       (CharParsing (string), Parser,
                                      TokenParsing (token), integer, some)

aoc15 :: IO ()
aoc15 = do
  --printSolutions 15 $ MkAoCSolution parseInput part1
  printTestSolutions 15 $ MkAoCSolution parseInput part2

data Sensor
  = MkSensor
      { _sensor    :: !Point
      , _beacon    :: !Point
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

part1 = solve 2000000

part2 sensors = traceLns (renderVectorSet pts) $ aSensor
  where aSensor = sensors !! 6
        pts = boundaryPoints aSensor

solve yVal sensors = length $ S.difference cc beaconsOnRow
  where cc = combinedCrossSections yVal sensors
        beaconsOnRow = S.fromList $ map (^. _x) $ filter (\b -> b ^. _y == yVal) $ map _beacon sensors

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
boundaryPoints :: Sensor -> S.Set Point
boundaryPoints MkSensor{..} = S.fromList allPoints
  where mDistance = manhattanDistance _sensor _beacon
        justTopRight = zipWith V2 (flexibleRange 0 (mDistance + 1)) (flexibleRange (mDistance +1) 0)
        allVectors = concatMap (take 4 . iterate perp) justTopRight
        allPoints = map (+ _sensor) allVectors

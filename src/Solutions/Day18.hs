{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solutions.Day18 where
import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Control.Lens.Getter     ((^.))
import           Control.Monad.Loops     (concatM)
import           Control.Monad.RWS       (MonadReader (ask))
import           Control.Monad.Reader    (Reader, asks, runReader)
import           Data.Finite             (Finite, finite)
import           Data.Foldable           (find, maximumBy)
import           Data.Function           (on)
import           Data.Map.Lazy           (Map)
import qualified Data.Map.Lazy           as M
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Debug.Trace             (trace, traceShow)
import           Foreign                 (free)
import           Linear.V3               (R1 (_x), R2 (_y), R3 (_z), V3 (..))
import           Linear.Vector           (unit)
import           Text.Parser.Combinators (some)
import           Text.Parser.Token       (integer)
import           Text.Printf             (printf)
import           Text.Trifecta           (Parser, commaSep)

data CountState
  = MkCountState
      { _insideFreeFace  :: !(Set Cube)
      , _outsideFreeFace :: !(Set Cube)
      }
  deriving (Eq, Show)

aoc18 :: IO ()
aoc18 = do
  --printTestSolutions 18 $ MkAoCSolution parseInput part1
  printTestSolutions 18 $ MkAoCSolution parseInput part2

type Cube = V3 Integer
type Grid = Set Cube
type Faces = Finite 7

parseInput :: Parser (Set Cube)
parseInput = do
  S.fromList <$> some parseCube
  where parseCube = do
          [x,y,z] <- commaSep integer
          pure $ V3 x y z

--part1 :: Grid -> Integer
part1 = solve

part2 grid = memo M.! V3 1 1 1
  where memo = inflateK grid

solve grid = sum $ map (toInteger . numFreeFaces grid) $ S.toList grid

numFreeFaces :: Grid -> Cube -> Faces
numFreeFaces grid cube = finite $ toInteger $ length $ freeFaces grid cube

freeFaces :: Set Cube -> Cube -> Set Cube
freeFaces grid cube = neighbours cube `S.difference` grid

neighbours :: Cube -> Set Cube
neighbours point = S.fromList $ concatMap (\u -> [point + u, point - u]) units
  where
    units :: [V3 Integer] = [unit _x, unit _y, unit _z]


--For each cube: 1. get all free spaces around it
--2. Figure out if space can reach the outside. If yes, count. if no, ignore
countOuterFreeFaces :: Grid -> Integer
countOuterFreeFaces grid = toInteger $ length outsideFreeFaces
  where isOutsideFreeFace face = memo M.! face
        memo = inflateK grid
        allFreeFaces = S.unions $ S.map (freeFaces grid) grid
        outsideFreeFaces = S.filter isOutsideFreeFace allFreeFaces

inflateK :: Grid -> Map Cube Bool
inflateK grid = memo
  where memo = M.fromList $ map (\c -> (c, go c S.empty)) $ enumerateBounds grid
        go :: Cube -> Set Cube -> Bool
        go cube' seen = traceShow cube' $ M.findWithDefault (inflateOneLayer cube' seen) cube' memo
        inflateOneLayer :: Cube -> Set Cube -> Bool
        inflateOneLayer cube' seen
            | any (outsideBounds grid) reduced = True --Can get to the outside
            | null reduced = False --Couldn't expand any more
            | otherwise = any (`go` S.insert cube' seen) reduced
          where expanded = S.filter (not . overlaps grid) $ neighbours cube'
                reduced = S.difference expanded seen

outsideBounds :: Grid -> Cube -> Bool
outsideBounds grid (V3 x y z) = x > maxX || x < minX || y > maxY || y < minY || z > maxZ || z < minZ
  where maxDimension dimension = maximum $ S.map dimension grid
        minDimension dimension = minimum $ S.map dimension grid
        [maxX, maxY, maxZ] = map maxDimension [(^. _x), (^. _y), (^. _z)]
        [minX, minY, minZ] = map minDimension [(^. _x), (^. _y), (^. _z)]

--refactor this!
--this does not actually enumerate all bounds yet. Not sure why
enumerateBounds :: Grid -> [Cube]
enumerateBounds grid = [V3 x y z | x <- [minX..maxX], y <- [minY, maxY], z <- [minZ, maxZ]]
  where maxDimension dimension = maximum $ S.map dimension grid
        minDimension dimension = minimum $ S.map dimension grid
        [maxX, maxY, maxZ] = map maxDimension [(^. _x), (^. _y), (^. _z)]
        [minX, minY, minZ] = map minDimension [(^. _x), (^. _y), (^. _z)]

--I can probably inline this
overlaps :: Grid -> Cube -> Bool
overlaps grid cube = cube `S.member` grid

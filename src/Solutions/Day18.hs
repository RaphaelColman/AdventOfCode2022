{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solutions.Day18 where
import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Control.Lens.Getter     ((^.))
import           Control.Monad.Loops     (concatM)
import           Control.Monad.RWS       (MonadReader (ask))
import           Control.Monad.Reader    (Reader, asks, runReader)
import           Control.Monad.State
import           Data.Finite             (Finite, finite)
import           Data.Foldable           (find, maximumBy)
import           Data.Function           (on, (&))
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

aoc18 :: IO ()
aoc18 = do
  --printTestSolutions 18 $ MkAoCSolution parseInput part1
  printSolutions 18 $ MkAoCSolution parseInput part2

type Cube = V3 Integer
type Grid = Set Cube
type Faces = Finite 7

data CubeMap
  = MkCubeMap
      { _cubeMap    :: !(Map Cube Bool)
      , _inProgress :: !(Set Cube)
      }

parseInput :: Parser (Set Cube)
parseInput = do
  S.fromList <$> some parseCube
  where parseCube = do
          [x,y,z] <- commaSep integer
          pure $ V3 x y z

--part1 :: Grid -> Integer
part1 grid = sum $ map (toInteger . numFreeFaces grid) $ S.toList grid

part2 = countOuterFreeFaces

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
--countOuterFreeFaces :: Grid -> Integer
countOuterFreeFaces grid = length outsideFreeFaces
  where allFreeFaces = concatMap (S.toList . freeFaces grid) grid
        outsideFreeFaces = filter (canReachOutside grid) allFreeFaces

canReachOutside :: Grid -> Cube -> State (Map Cube Bool) Bool
canReachOutside grid cube = go (S.singleton cube) S.empty
  where go :: Set Cube -> Set Cube -> State (Map Cube Bool) Bool
        go cubes seen = do
            mp <- get
            let toSearch = S.difference cubes seen
            let (found, notFound) = S.partition (`M.member` mp) toSearch
            pure undefined
            -- | any (outsideBounds grid) newCubes = True --Can get to the outside
            -- | null newCubes = False --Couldn't expand any more
            -- | otherwise = go newCubes (S.union seen newCubes)
          where expanded = S.unions $ S.map (freeFaces grid) cubes
                newCubes = S.difference expanded seen

outsideBounds :: Grid -> Cube -> Bool
outsideBounds grid (V3 x y z) = x > maxX || x < minX || y > maxY || y < minY || z > maxZ || z < minZ
  where maxDimension dimension = maximum $ S.map dimension grid
        minDimension dimension = minimum $ S.map dimension grid
        [maxX, maxY, maxZ] = map maxDimension [(^. _x), (^. _y), (^. _z)]
        [minX, minY, minZ] = map minDimension [(^. _x), (^. _y), (^. _z)]

enumerateBounds :: Grid -> [Cube]
enumerateBounds grid = [V3 x y z | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]]
  where maxDimension dimension = maximum $ S.map dimension grid
        minDimension dimension = minimum $ S.map dimension grid
        [maxX, maxY, maxZ] = map maxDimension [(^. _x), (^. _y), (^. _z)]
        [minX, minY, minZ] = map minDimension [(^. _x), (^. _y), (^. _z)]

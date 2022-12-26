module Solutions.Day12
    ( aoc12
    ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.Debugging    (traceLns)
import           Common.Geometry     (Grid, Point,
                                      enumerateMultilineStringToVectorMap,
                                      gridOrthogonalNeighbours,
                                      manhattanDistance, renderVectorSet)
import           Control.Applicative (some)
import           Data.Char           (ord)
import           Data.Graph.AStar    (aStar)
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS
import qualified Data.Map            as M
import           Data.Maybe          (fromJust, mapMaybe)
import qualified Data.Set            as S
import           Linear              (V2 (V2))
import           Safe                (headMay)
import           Text.Parser.Char    (CharParsing (anyChar))
import           Text.Trifecta       (Parser)

aoc12 :: IO ()
aoc12 = do
  printSolutions 12 $ MkAoCSolution parseInput part1
  printSolutions 12 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Char)
parseInput = do
  allChars <- some anyChar
  pure $ enumerateMultilineStringToVectorMap allChars

part1 :: M.Map Point Char -> Maybe Int
part1 grid = length <$> solved
  where solved = solve grid start
        start = fromJust $ findPoint grid 'S'

part2 :: Grid Char -> Int
part2 = bestStartingPoint

getNeighbours :: Grid Char -> Point -> HashSet Point
getNeighbours grid pt = HS.fromList $ M.keys availableNeigbours
  where allNeighbours = gridOrthogonalNeighbours grid pt
        currentHeight = height $ grid M.! pt
        availableNeigbours = M.filter (\c ->
                                            height c <= currentHeight + 1
                                      ) allNeighbours

solve :: Grid Char -> Point -> Maybe [Point]
solve grid = aStar
              (getNeighbours grid) --node -> neighbours
              (\a b -> 1) -- node -> node -> cost
              (`manhattanDistance` goalPt) -- node -> heuristic cost to goal
              (\pt -> grid M.! pt == 'E') -- node -> bool (finishing node?)
      where goalPt = fromJust $ findPoint grid 'E' --Maybe using aStarM ?

bestStartingPoint :: Grid Char -> Int
bestStartingPoint grid = minimum paths
  where possibleStartingPoints = M.keys $ M.filter (\c -> c `elem` ['a', 'S']) grid
        paths = map length $ mapMaybe (solve grid) possibleStartingPoints

height :: Char -> Int
height 'E' = 26
height 'S' = 1
height c   = ord c  - 96

findPoint :: Grid Char -> Char -> Maybe Point
findPoint grid c = headMay $ M.keys $ M.filter (== c) grid

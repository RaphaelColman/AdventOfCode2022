module Solutions.Day8
    ( aoc8
    ) where

import           Common.AoCSolutions   (AoCSolution (MkAoCSolution),
                                        printSolutions, printTestSolutions)
import           Common.Geometry       (Grid, Point,
                                        enumerateMultilineStringToVectorMap)
import           Control.Applicative   (some)
import           Data.Functor.Base     (ListF (Nil))
import           Data.Functor.Foldable (Corecursive (ana, apo), ListF (Cons),
                                        unfold)
import           Data.List             (unfoldr)
import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe)
import           Linear                (unit)
import           Linear.V2             (R1 (_x), R2 (_y), V2 (V2))
import           Text.Parser.Char      (anyChar)
import           Text.Trifecta         (Parser)

aoc8 :: IO ()
aoc8 = do
  printSolutions 8 $ MkAoCSolution parseInput part1
  printSolutions 8 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Int)
parseInput = do
  chars <- some anyChar
  let g = enumerateMultilineStringToVectorMap chars
  pure $ M.map (\c -> read [c]) g

data Direction = RIGHT | DOWN | LEFT | UP deriving (Bounded, Enum, Eq, Show)

part1 :: Grid Int -> Int
part1 grid = numberVisible
  where numberVisible = length $ filter (isVisible grid) $ M.keys grid --This is really slow!

part2 :: Grid Int -> Int
part2 grid = maximum scores
  where scores = map (scenicScore grid) $ M.keys grid

isVisible :: Grid Int -> Point -> Bool
isVisible grid point@(V2 treex treey) = visible
  where pointHeight = grid M.! point
        unfoldInDirection :: Direction -> [Int]
        unfoldInDirection direction = ana (go direction) (neighbour point direction)
        go :: Direction -> Point -> ListF Int Point
        go direction point' = case M.lookup point' grid of
          Nothing     -> Nil
          Just height -> Cons height (neighbour point' direction)
        visible = any ((\treeHeights -> all (< pointHeight) treeHeights || null treeHeights) . unfoldInDirection) [RIGHT .. UP]

neighbour :: Point -> Direction -> Point
neighbour point@(V2 x y) direction = case direction of
  RIGHT -> point + unit _x
  DOWN  -> point + unit _y
  LEFT  -> point - unit _x
  UP    -> point - unit _y

treesVisibleDirection :: Grid Int -> Point -> Direction -> Int
treesVisibleDirection grid point direction = length shorterTrees
  where pointHeight = grid M.! point
        shorterTrees = unfoldr go (Just (neighbour point direction))
        --If we find a tree which is too high we still want to count it, so put a Nothing in the second element of the tuple
        --which will signal the next iteration to stop
        go :: Maybe Point -> Maybe (Point, Maybe Point)
        go point' = do
          unwrapped <- point'
          foundHeight <- M.lookup unwrapped grid
          let nextPoint = neighbour unwrapped direction
          if foundHeight >= pointHeight
            then Just (nextPoint, Nothing)
            else Just (nextPoint, Just nextPoint)

scenicScore :: Grid Int -> Point -> Int
scenicScore grid point = product $ map (treesVisibleDirection grid point) [UP, LEFT, RIGHT, DOWN]

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Solutions.Day9 where

import           Common.AoCSolutions      (AoCSolution (MkAoCSolution),
                                           printSolutions, printTestSolutions)
import           Common.Debugging         (traceLns)
import           Common.Geometry          (Point, neighbours, renderVectorList,
                                           renderVectorMap, renderVectorSet)
import           Common.ListUtils         (window2)
import           Control.Monad.State      (runState)
import           Control.Monad.State.Lazy (modify)
import           Data.Foldable            (Foldable (foldl'))
import qualified Data.Foldable            as S
import           Data.Function            ((&))
import           Data.List                (nub)
import qualified Data.Set                 as S
import           Linear                   (Metric (norm), V2)
import           Linear.V2                (R1 (_x), R2 (_y), V2 (V2))
import           Linear.Vector            (unit)
import           Text.Trifecta            (CharParsing (anyChar), Parser,
                                           integer, some, space)

aoc9 :: IO ()
aoc9 = do
  printSolutions 9 $ MkAoCSolution parseInput part1
  printSolutions 9 $ MkAoCSolution parseInput part2

data Direction = NORTH | EAST | SOUTH | WEST deriving (Bounded, Enum, Eq, Show)
data Instruction
  = MkInstruction
      { _direction :: !Direction
      , _amount    :: !Int
      }
  deriving (Eq, Show)

parseInput :: Parser [Instruction]
parseInput = some parseInstruction
  where parseInstruction :: Parser Instruction
        parseInstruction = do
          directionChar <- anyChar
          direction <- case directionChar of
                            'U' -> pure NORTH
                            'R' -> pure EAST
                            'D' -> pure SOUTH
                            'L' -> pure WEST
                            _   -> fail "Unexpected character"
          space
          MkInstruction direction . fromInteger <$> integer

part1 :: [Instruction] -> Int
part1 = solve 2

part2 :: [Instruction] -> Int
part2 = solve 10

solve :: Foldable t => Int -> t Instruction -> Int
solve ropeLength instructions = foldl' applyInstruction initial instructions
                    & _tailPositions
                    & length
  where initial = MkState (replicate ropeLength (V2 0 0)) S.empty


data RopeState
  = MkState
      { _knots         :: ![V2 Int]
      , _tailPositions :: !(S.Set (V2 Int))
      }
  deriving (Eq, Show)

updateHead :: Direction -> RopeState -> RopeState
updateHead direction state@(MkState (headKnot : otherKnots) _) = state { _knots = headKnot + getUnitVector direction : otherKnots }

moveTail :: Point -> Point -> Point
moveTail headKnot tailKnot = tailKnot + tailMovement
  where diff = headKnot - tailKnot
        normalize x = if x == 0 then 0 else x `div` abs x
        tailMovement = if touching headKnot tailKnot
                       then V2 0 0
                       else normalize <$> diff

updateTails :: RopeState -> RopeState
updateTails state@(MkState knots@(headKnot:otherKnots) tailPositions) = MkState knewKnots (last knots `S.insert` tailPositions)
  where knewKnots = reverse $ foldl' (\ks@(headK:_) tailK -> moveTail headK tailK:ks) [headKnot] otherKnots

touching :: Point -> Point -> Bool
touching p1 p2 = p1 `elem` neighbours p2 || p1 == p2

applySingleInstruction :: Direction -> RopeState -> RopeState
applySingleInstruction direction state@MkState{..} = snd . flip runState state $ do
                modify $ updateHead direction
                modify updateTails

applyInstruction :: RopeState -> Instruction -> RopeState
applyInstruction state MkInstruction{..} = iterate (applySingleInstruction _direction) state !! _amount


getUnitVector :: Direction -> V2 Int
getUnitVector direction = case direction of
  NORTH -> - unit _y
  EAST  -> unit _x
  SOUTH -> unit _y
  WEST  -> - unit _x

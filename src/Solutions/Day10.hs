{-# LANGUAGE ScopedTypeVariables #-}
module Solutions.Day10 where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Common.Geometry         (Point, renderVectorMap)
import           Control.Applicative     (some)
import           Data.Foldable           (Foldable (foldl', toList))
import qualified Data.Foldable           as Seq
import           Data.Functor            (($>))
import qualified Data.Map                as M
import           Data.Sequence           (ViewR ((:>)), (|>))
import qualified Data.Sequence           as Seq
import           GHC.Base                (Alternative ((<|>)))
import           Linear                  (V2 (V2))
import           Linear.V2               (R1 (_x), V2 (V2))
import           Text.Parser.Char        (CharParsing (string))
import           Text.Parser.Combinators (try)
import           Text.Parser.Token       (TokenParsing (token), integer)
import           Text.Trifecta           (Parser, integer)
import Control.Lens.Getter ((^.))
import Common.Debugging (traceLns)

data Instruction
  = Addx !Integer
  | Noop
  deriving (Eq, Show)

type CpuState = Seq.Seq Integer

aoc10 :: IO ()
aoc10 = do
  printSolutions 10 $ MkAoCSolution parseInput part1
  printSolutions 10 $ MkAoCSolution parseInput part2

parseInput :: Parser [Instruction]
parseInput = do
  some $ token parseInstruction

parseInstruction :: Parser Instruction
parseInstruction = do
  try parseAddx <|> try parseNoop
  where parseAddx = Addx <$> (string "addx " *> integer)
        parseNoop = string "noop" $> Noop

part1 :: [Instruction] -> Maybe Integer
part1 = extractSignalStrength . applyInstructions

part2 :: [Instruction] -> String
part2 instructions = traceLns (renderVectorMap grid) ""
  where grid = drawPixels instructions

extractSignalStrength :: Seq.Seq Integer -> Maybe Integer
extractSignalStrength state = do
  let cycleNumbers :: [Integer] = [20, 60, 100, 140, 180, 220]
  registers <- traverse ((`Seq.lookup` state) . fromInteger . (\x -> x - 1)) cycleNumbers
  pure $ sum $ zipWith (*) cycleNumbers registers

applyInstructions :: [Instruction] -> CpuState
applyInstructions = foldl' doInstruction $ Seq.singleton 1
  where doInstruction :: CpuState -> Instruction -> CpuState
        doInstruction state instr = case instr of
          Addx n -> let newReg = register + n in state |> register |> newReg
          Noop   -> state |> register
          where _ :> register = Seq.viewr state

pixels :: [Point]
pixels = [V2 x y | y <- [0..height-1], x <- [0..width-1]]
  where (width, height) = (40, 6)


drawPixels :: [Instruction] -> M.Map Point Char
drawPixels instructions = grid
  where finishedState :: [Int] = map fromInteger $ toList $ applyInstructions instructions
        grid = M.fromList $ zipWith combine pixels finishedState
        combine :: Point -> Int -> (Point, Char)
        combine pt register = if (pt ^. _x) `elem` [register-1, register, register+1]
                              then (pt, '#')
                              else (pt, '.')

{-# LANGUAGE ScopedTypeVariables #-}
module Solutions.Day10
    ( aoc10
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Control.Applicative     (some)
import           Data.Foldable           (Foldable (foldl'))
import           Data.Functor            (($>))
import           Data.Sequence           (ViewR ((:>)), (|>))
import qualified Data.Sequence           as Seq
import           GHC.Base                (Alternative ((<|>)))
import           Text.Parser.Char        (CharParsing (string))
import           Text.Parser.Combinators (try)
import           Text.Parser.Token       (TokenParsing (token), integer)
import           Text.Trifecta           (Parser, integer)

data Instruction
  = Addx !Integer
  | Noop
  deriving (Eq, Show)

type CpuState = Seq.Seq Integer

aoc10 :: IO ()
aoc10 = do
  --printSolutions 10 $ MkAoCSolution parseInput part1
  printSolutions 10 $ MkAoCSolution parseInput part2

parseInput :: Parser [Instruction]
parseInput = do
  some $ token parseInstruction

parseInstruction :: Parser Instruction
parseInstruction = do
  try parseAddx <|> try parseNoop
  where parseAddx = Addx <$> (string "addx " *> integer)
        parseNoop = string "noop" $> Noop

part1 = extractSignalStrength . applyInstructions

part2 = id

extractSignalStrength :: Seq.Seq Integer -> Maybe Integer
extractSignalStrength state = do
  let cycleNumbers :: [Integer] = [20, 60, 100, 140, 180, 220]
  registers <- traverse ((`Seq.lookup` state) . fromInteger . (\x -> x - 1)) cycleNumbers
  --pure $ zip cycleNumbers registers
  pure $ sum $ zipWith (*) cycleNumbers registers

applyInstructions :: [Instruction] -> CpuState
applyInstructions = foldl' doInstruction $ Seq.singleton 1
  where doInstruction :: CpuState -> Instruction -> CpuState
        doInstruction state instr = case instr of
          Addx n -> let newReg = register + n in state |> register |> newReg
          Noop   -> state |> register
          where _ :> register = Seq.viewr state

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Solutions.Day5
    ( aoc5
    ) where

import           Common.AoCSolutions               (AoCSolution (MkAoCSolution),
                                                    printSolutions,
                                                    printTestSolutions)
import           Common.FunctorUtils               (fmap2)
import           Control.Applicative               (Alternative (some), (<|>))
import           Control.Applicative.Combinators   (skipManyTill)
import           Control.Lens                      (simple)
import           Control.Monad.State               (MonadState (get), runState)
import           Control.Monad.State.Lazy          (modify)
import           Data.Foldable                     (Foldable (foldl'))
import           Data.Functor                      (($>))
import qualified Data.IntMap                       as IM
import           Data.Maybe                        (mapMaybe)
import           System.Directory.Internal.Prelude (catMaybes)
import           Text.Parser.Combinators           (many)
import           Text.Parser.Token                 (Unlined (Unlined, runUnlined),
                                                    Unspaced (..), integer,
                                                    stringLiteral, token)
import           Text.Trifecta                     (CharParsing (anyChar, char, string),
                                                    Parser, Parsing (eof, try),
                                                    Unspaced (runUnspaced),
                                                    brackets, count, letter,
                                                    manyTill, newline, sepBy,
                                                    space)

aoc5 :: IO ()
aoc5 = do
  printSolutions 5 $ MkAoCSolution parseInput part1
  printSolutions 5 $ MkAoCSolution parseInput part2

type StackSlice = IM.IntMap Char
type Stack = IM.IntMap [Char]
data Instruction
  = MkInstruction
      { amount :: !Int
      , from   :: !Int
      , to     :: !Int
      }
  deriving (Eq, Show)

parseInput :: Parser (Stack, [Instruction])
parseInput = do
  stacks <- fmap2 (:[]) <$> some parseCrateLine
  let combined = IM.unionsWith (++) stacks
  skipManyTill anyChar newline
  skipManyTill anyChar newline
  instructions <- some $ token parseInstruction
  pure (combined, instructions)

parseCrateSpace :: Parser (Maybe Char)
parseCrateSpace = do
  try (Just <$> parseCrate) <|> try (parseSpace $> Nothing)
  where parseCrate :: Parser Char
        parseCrate = do
          char '['
          l <- letter
          char ']'
          pure l
        parseSpace :: Parser [Char]
        parseSpace = count 3 space


parseCrateLine :: Parser StackSlice
parseCrateLine = do
  c <- sepBy parseCrateSpace (char ' ')
  let l = catMaybes $ zipWith (\i maybeCrate -> (i, ) <$> maybeCrate) [1..] c
  newline
  pure $ IM.fromList l

parseInstruction :: Parser Instruction
parseInstruction = do
  string "move "
  amount <- fromInteger <$> integer
  string "from "
  from <- fromInteger <$> integer
  string "to "
  MkInstruction amount from . fromInteger <$> integer

part1 :: Foldable t => (Stack, t Instruction) -> [Char]
part1 (stack, instructions) = IM.foldr (\a b -> head a : b) "" runInstructions
  where runInstructions = foldl' (flip (applyInstruction True)) stack instructions

part2 :: Foldable t => (Stack, t Instruction) -> [Char]
part2 (stack, instructions) = IM.foldr (\a b -> head a : b) "" runInstructions
  where runInstructions = foldl' (flip (applyInstruction False)) stack instructions

applyInstruction :: Bool -> Instruction -> Stack -> Stack
applyInstruction withReverse MkInstruction{..} stack = snd . flip runState stack $ do
          current <- get
          let cratesToMove = take amount $ current IM.! from
          let cratesToDrop = if withReverse then reverse cratesToMove else cratesToMove
          modify $ IM.adjust (drop amount) from
          modify $ IM.adjust (cratesToDrop ++) to

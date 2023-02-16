{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Solutions.Day19
    ( aoc19
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Control.Lens.Getter     ((^.))
import           Control.Monad.Loops     (concatM)
import           Control.Monad.RWS       (MonadReader (ask))
import           Control.Monad.Reader    (Reader, asks, runReader)
import           Data.Finite             (Finite, finite)
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Linear.V3               (R1 (_x), R2 (_y), R3 (_z), V3 (..))
import           Linear.Vector           (unit)
import           Text.Parser.Combinators (some)
import           Text.Parser.Token       (integer)
import           Text.Trifecta           (Parser, commaSep)

aoc19 :: IO ()
aoc19 = do
  printTestSolutions 19 $ MkAoCSolution parseInput part1
  --printSolutions 19 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = undefined

part1 = id


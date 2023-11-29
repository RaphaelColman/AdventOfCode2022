{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Solutions.Day19
    ( aoc19
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Control.Lens            (makeLenses)
import           Control.Lens.Getter     ((^.))
import           Control.Monad.Loops     (concatM)
import           Control.Monad.RWS       (MonadReader (ask))
import           Control.Monad.Reader    (Reader, asks, runReader)
import           Data.Finite             (Finite, finite)
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Set                (Set)
import qualified Data.Set                as S
import           Linear.V3               (R1 (_x), R2 (_y), R3 (_z), V3 (..))
import           Linear.Vector           (unit)
import           Text.Parser.Combinators (some)
import           Text.Parser.Token       (integer)
import           Text.Trifecta           (CharParsing (string), Parser,
                                          commaSep)
import Text.Parser.Char (letter)

type Blueprint = Map Material (Map Material Integer)

type Ingredients = Map Material Integer
data Material = Ore | Clay | Obsidian | Geode deriving (Enum, Eq, Ord, Show)

aoc19 :: IO ()
aoc19 = do
  printTestSolutions 19 $ MkAoCSolution parseInput part1
  --printSolutions 19 $ MkAoCSolution parseInput part2

parseInput :: Parser Blueprint
parseInput = do
  string "Blueprint 1: "
  ore <- parseRobot "ore"
  clay <- parseRobot "clay"
  obsidian <- parseRobot "obsidian"
  geode <- parseRobot "geode"
  pure undefined
  where parseRobot :: String -> Parser (Map Material Integer)
        parseRobot t = do
          string "Each " >> string t >> string " robot costs "
          i <- integer
          string "ore. "
          pure undefined

parseIngredients :: Parser [(Integer, Material)]
parseIngredients = do
  amount <- integer
  material <- parseMaterial
  pure [(amount, material)]
  where parseMaterial :: Parser Material
        parseMaterial = do
          l <- some letter
          case l of
            "ore" -> pure Ore
            "clay" -> pure Clay
            "obsidian" -> pure Obsidian
            "geode" -> pure Geode
            _ -> fail "unrecognised material"


part1 = id


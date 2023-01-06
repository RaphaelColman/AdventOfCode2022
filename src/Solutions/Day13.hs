module Solutions.Day13
    ( aoc13
    ) where

import           Common.AoCSolutions     (AoCSolution (MkAoCSolution),
                                          printSolutions, printTestSolutions)
import           Common.FunctorUtils     (fmap2)
import           Control.Applicative     (Alternative ((<|>)))
import           Data.List               (elemIndex, sort)
import           Text.Parser.Combinators (Parsing (try), count)
import           Text.Trifecta           (Parser, brackets, commaSep, integer,
                                          parens, some)

data Value
  = VInt !Integer
  | VList ![Value]
  deriving (Eq, Show)

instance Ord Value where
  compare left@(VList _) right@(VInt _) = compare left (VList [right])
  compare left@(VInt _) right@(VList _) = compare (VList [left]) right
  compare left right    = compare left right

type PacketPair = (Value, Value)

aoc13 :: IO ()
aoc13 = do
  printSolutions 13 $ MkAoCSolution parsePacketPairs part1
  printSolutions 13 $ MkAoCSolution parseAllPackets part2

parsePacketPairs :: Parser [PacketPair]
parsePacketPairs = some parsePair
  where parsePair = do
          [p1, p2] <- count 2 parseVList
          pure (p1, p2)

parseAllPackets :: Parser [Value]
parseAllPackets = some parseVList

parseVList :: Parser Value
parseVList = do
  values <- brackets (commaSep go)
  pure $ VList values
  where go = try parseVList <|> try parseVInt
        parseVInt = VInt <$> integer

part1 :: [PacketPair] -> Integer
part1 = sum . map fst . filter ((==LT) . snd) . zip [1..] . map (uncurry compare)

part2 :: [Value] -> Maybe Int
part2 packets = product <$> fmap2 (+1) indices
  where ordered = orderPackets packets
        indices = traverse (`elemIndex` ordered) [extra2, extra6]

orderPackets :: [Value] -> [Value]
orderPackets packets = sort $ extra2:extra6:packets

extra2 = VList [VList [VInt 2]]
extra6 = VList [VList [VInt 6]]

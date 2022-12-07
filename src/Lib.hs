module Lib where

import           Data.Functor.Base     (TreeF (NodeF))
import           Data.Functor.Foldable
import           Data.Tree             (Tree, unfoldTree)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

depth :: Tree a -> Int
depth = cata go
  where go :: TreeF a Int -> Int
        go (NodeF _ []) = 1
        go (NodeF _ subDepths) = 1 + maximum subDepths

halves :: Int -> [Int]
halves = ana go
  where go :: Int -> ListF Int Int
        go 0 = Nil
        go n = Cons n (n `div` 2)

aTree = unfoldTree buildNode 1
  where buildNode :: Int -> (Int, [Int])
        buildNode x = if x*5 > 30
                      then (x, [])
                      else (x, [x*2, x*3, x*5])

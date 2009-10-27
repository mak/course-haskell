-- module FBList where

import Data.List

foo :: Int -> Int
foo n = foldr (+) 0  $ map (uncurry (*))  $ repeat 42 `zip` enumFromTo 1 n
main = print =<< (foo . read  ) `fmap`  getLine

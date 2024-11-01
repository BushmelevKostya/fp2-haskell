module Main (main) where

import Lib

increment :: Int -> Int
increment v = v + 1

main :: IO ()
main = do
  let
    set = createSet :: SCSet Int
    newSet = foldr add set [9, 1, 2, 1, 9, 17, 25]
    new1Set = filterValues even newSet
  print newSet
  print new1Set

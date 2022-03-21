module Main where

import Lib

main :: IO ()
main = do
  print $ add 2 (add 1 empty)
  print $ union 2 3 empty
  print $ union 2 3 (add 2 (add 1 empty))

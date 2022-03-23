{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.ST (ST, runST)
import Lib

main :: IO ()
main = do
  --   print $ add 2 (add 1 empty)
  --   print $ union 2 3 empty
  --   print $ union 2 3 (add 2 (add 1 empty))
  print (runST $ empty >>= toLists :: [[Int]])
  print $
    runST $
      do
        d <- empty
        add 1 d
        toLists d

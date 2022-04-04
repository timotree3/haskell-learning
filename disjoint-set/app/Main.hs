-- {-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.ST (ST, runST)
import Lib

main :: IO ()
main = do
  --   print $ union 2 3 empty
  --   print $ union 2 3 (add 2 (add 1 empty))
  print (runST $ empty >>= immut :: IDisjointSet Int)
  print $
    runST $
      do
        d <- empty
        add 1 d
        add 2 d
        immut d
  print $
    runST $
      do
        d <- empty
        add 1 d
        add 2 d
        add 3 d
        union 1 2 d
        add 4 d
        union 2 4 d
        immut d

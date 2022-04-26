-- {-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.ST (ST, runST)
import Lib

main :: IO ()
main = do
  putStrLn $
    runST $
      do
        d <- empty
        add (1 :: Int) d
        add 2 d
        showSt d
  putStrLn $
    runST $
      do
        d <- empty
        add (1 :: Int) d
        add 2 d
        add 3 d
        union 1 2 d
        add 4 d
        union 2 4 d
        showSt d

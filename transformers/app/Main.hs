module Main where

import Data.Maybe (fromMaybe)
import Lib
import Text.Read (readMaybe)

main :: IO ()
main = do
  maybe <- runMaybeT useMaybeT
  case maybe of
    Just () -> putStrLn "success!"
    Nothing -> putStrLn "failure.."

useMaybeT :: MaybeT IO ()
useMaybeT = do
  name <- lift getLine
  number <- lift getLine
  number' <- liftMaybe $ readMaybe number
  lift $ print name
  lift $ print (number' :: Int)
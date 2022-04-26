{-# LANGUAGE NamedFieldPuns #-}

module Lib
  ( add,
    empty,
    union,
    DisjointSet,
    showSt,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Control.Monad.ST
import Data.Foldable (asum)
import qualified Data.HashTable.Class as HashTable
import Data.HashTable.ST.Basic (HashTable)
import Data.Hashable (Hashable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.STRef

newtype DisjointSet s a = DisjointSet {unDisjointSet :: HashTable s a (Node a)}

-- Rep = Representative node
data Node a = Rep {rank :: Int} | Child {parent :: a} deriving (Show)

empty :: ST s (DisjointSet s a)
empty = DisjointSet <$> HashTable.new

add :: (Hashable a, Eq a) => a -> DisjointSet s a -> ST s ()
add x (DisjointSet m) = HashTable.mutate m x (\n -> (n <|> Just Rep {rank = 0}, ()))

same :: (Hashable a, Eq a) => a -> a -> DisjointSet s a -> ST s Bool
same x y d = (==) <$> find x d <*> find y d

find :: (Hashable a, Eq a) => a -> DisjointSet s a -> ST s (Maybe a)
find x (DisjointSet m) = do
  n <- HashTable.lookup m x
  case n of
    Just (Rep _) -> pure $ Just x
    Just (Child p) -> do
      mR <- find p (DisjointSet m)
      case mR of
        Just r -> setParent x r (DisjointSet m)
        Nothing -> pure ()
      pure mR
    Nothing -> pure Nothing

showSt :: (Show a) => DisjointSet s a -> ST s String
showSt (DisjointSet m) =
  show <$> HashTable.toList m

union :: (Hashable a, Eq a) => a -> a -> DisjointSet s a -> ST s ()
union x y d = do
  mRepX <- find x d
  mRepY <- find y d
  case (mRepX, mRepY) of
    (Just repX, Just repY) -> mergeReps repX repY d
    _ -> error "bad args to union"

-- Precondition: x and y are both `Rep`
mergeReps :: (Hashable a, Eq a) => a -> a -> DisjointSet s a -> ST s ()
mergeReps x y d = do
  (toDemote, toKeep) <- toDemoteAndToKeep x y d
  setParent toDemote toKeep d
  modifyRank (+ 1) toKeep d

setParent :: (Hashable a, Eq a) => a -> a -> DisjointSet s a -> ST s ()
setParent child parent (DisjointSet m) = HashTable.insert m child Child {parent}

-- One of the two representatives has to become a normal node,
-- making all of its children have a longer route.
-- The representative with lower rank has less children,
-- and therefore should not remain a representative
toDemoteAndToKeep :: (Hashable a, Eq a) => a -> a -> DisjointSet s a -> ST s (a, a)
toDemoteAndToKeep x y d = sort2ByKeyM (lookupRank d) (x, y)

extractRank :: Maybe (Node a) -> Int
extractRank (Just (Rep {rank})) = rank
extractRank _ = undefined

-- Precondition: x is in m and is `Rep`
lookupRank :: (Hashable a, Eq a) => DisjointSet s a -> a -> ST s Int
lookupRank (DisjointSet m) x = do
  extractRank <$> HashTable.lookup m x

-- Precondition: x is in m and is `Rep`
modifyRank :: (Hashable a, Eq a) => (Int -> Int) -> a -> DisjointSet s a -> ST s ()
modifyRank f x (DisjointSet m) = HashTable.mutate m x (\n -> (Just Rep {rank = f (extractRank n)}, ()))

sort2ByKeyM :: (Ord b, Applicative m) => (a -> m b) -> (a, a) -> m (a, a)
sort2ByKeyM f (x, y) = (\k k' -> if k <= k' then (x, y) else (y, x)) <$> f x <*> f y

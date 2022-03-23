{-# LANGUAGE NamedFieldPuns #-}

module Lib
  ( add,
    empty,
    union,
    toLists,
    DisjointSet,
  )
where

import Control.Monad (filterM)
import Control.Monad.ST
import Data.Foldable (asum)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.STRef

type NodeRef s a = STRef s (Node s a)

newtype DisjointSet s a = DisjointSet {unDisjointSet :: STRef s (Map a (NodeRef s a))}

data Node s a = Node {value :: a, parent :: Maybe (NodeRef s a), rank :: Int}

empty :: ST s (DisjointSet s a)
empty = DisjointSet <$> newSTRef Map.empty

add :: Ord a => a -> DisjointSet s a -> ST s ()
add x (DisjointSet rm) = do
  m <- readSTRef rm
  case Map.lookup x m of
    Just _ -> pure ()
    Nothing -> do
      n <- newSTRef Node {value = x, parent = Nothing, rank = 0}
      writeSTRef rm (Map.insert x n m)

union :: Ord a => a -> a -> DisjointSet s a -> ST s ()
union = undefined

find :: Ord a => a -> DisjointSet s a -> ST s (Maybe (Node s a))
find x (DisjointSet rm) = do
  m <- readSTRef rm
  case Map.lookup x m of
    Just node -> Just <$> (grandestParent =<< readSTRef node)
    Nothing -> pure Nothing

toLists :: Ord a => DisjointSet s a -> ST s [[a]]
toLists (DisjointSet rm) = do
  m <- readSTRef rm
  nodes <- traverse readSTRef (snd <$> Map.toList m)
  let leafs = value <$> filter isLeaf nodes
  descendantsPerLeaf <- traverse (`allDescendants` nodes) leafs
  pure $ fmap value <$> descendantsPerLeaf

-- NB: isDescendant x Node { value = x } == True
isDescendant :: Ord a => a -> Node s a -> ST s Bool
isDescendant leaf n = do
  gp <- grandestParent n
  return $ leaf == value gp

allDescendants :: Ord a => a -> [Node s a] -> ST s [Node s a]
allDescendants = filterM . isDescendant

isLeaf :: Node s a -> Bool
isLeaf Node {parent} = isNothing parent

grandestParent :: Node s a -> ST s (Node s a)
grandestParent node = case parent node of
  Just parent -> readSTRef parent >>= grandestParent
  Nothing -> pure node

representativeElementIfMember :: Eq a => a -> [a] -> Maybe a
representativeElementIfMember x s =
  if x `elem` s
    then Just (head s)
    else Nothing

predToMaybe :: (a -> Bool) -> a -> Maybe a
predToMaybe p x = if p x then Just x else Nothing

findMap :: (a -> Maybe t) -> [a] -> Maybe t
findMap p s = asum (p <$> s)
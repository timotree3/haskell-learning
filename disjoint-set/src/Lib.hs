{-# LANGUAGE NamedFieldPuns #-}

module Lib
  ( add,
    empty,
    union,
    toLists,
    DisjointSet,
    IDisjointSet,
    INode,
    immut,
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

newtype IDisjointSet a = IDisjointSet {unIDisjointSet :: Map a (INode a)} deriving (Show)

data INode a = IRep {iValue :: a, iRank :: Int} | IChild {iParent :: a} deriving (Show)

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
union x y d = do
  nn <- findBothRepresentatives
  (toDemote, toKeep) <- findRepToDemoteAndToKeep nn
  setParent toDemote toKeep
  modifyRank (+ 1) toKeep
  where
    findBothRepresentatives = do
      mn <- find x d
      mn' <- find y d
      case (mn, mn') of
        (Just nr, Just nr') -> pure (nr, nr')
        _ -> error "bad args to union"
    findRepToDemoteAndToKeep =
      -- One of the two representatives has to become a normal node,
      -- making all of its children have a longer route.
      -- The representative with lower rank has less children,
      -- and therefore should not remain a representative
      sort2ByKeyM ((rank <$>) . readSTRef)

sort2ByKeyM :: (Ord b, Applicative m) => (a -> m b) -> (a, a) -> m (a, a)
sort2ByKeyM f (x, y) = (\k k' -> if k <= k' then (x, y) else (y, x)) <$> f x <*> f y

mapParent :: (Maybe (NodeRef s a) -> Maybe (NodeRef s a)) -> Node s a -> Node s a
mapParent f n = Node {value = value n, parent = f (parent n), rank = rank n}

mapRank :: (Int -> Int) -> Node s a -> Node s a
mapRank f n = Node {value = value n, parent = parent n, rank = f (rank n)}

setParent :: NodeRef s a -> NodeRef s a -> ST s ()
setParent nr p =
  modifySTRef' nr $ mapParent $ const $ Just p

modifyRank :: (Int -> Int) -> NodeRef s a -> ST s ()
modifyRank f nr =
  modifySTRef' nr $ mapRank f

find :: Ord a => a -> DisjointSet s a -> ST s (Maybe (NodeRef s a))
find x (DisjointSet rm) = do
  m <- readSTRef rm
  case Map.lookup x m of
    Just node -> Just <$> grandestParent node
    Nothing -> pure Nothing

immut :: DisjointSet s a -> ST s (IDisjointSet a)
immut (DisjointSet rm) =
  do
    m <- readSTRef rm
    mi <- traverse iNode m
    pure $ IDisjointSet mi

iNode :: NodeRef s a -> ST s (INode a)
iNode nr =
  do
    Node {value = v, parent, rank} <- readSTRef nr
    case parent of
      Just pr -> do
        p <- readSTRef pr
        pure $ IChild {iParent = value p}
      Nothing -> pure IRep {iValue = v, iRank = rank}

toLists :: Ord a => DisjointSet s a -> ST s [[a]]
toLists (DisjointSet rm) = do
  m <- readSTRef rm
  let rns = snd <$> Map.toList m
  ns <- traverse readSTRef rns
  let leafs = value <$> filter isLeaf ns
  traverse (`allDescendants` rns) leafs

isDescendant :: Ord a => a -> NodeRef s a -> ST s Bool
isDescendant leaf rn =
  do
    n <- readSTRef rn
    if leaf == value n
      then pure False
      else do
        rgp <- grandestParent rn
        gp <- readSTRef rgp
        return $ leaf == value gp

allDescendants :: Ord a => a -> [NodeRef s a] -> ST s [a]
allDescendants leaf nodes = do
  d <- filterM (isDescendant leaf) nodes
  d' <- traverse readSTRef d
  pure (leaf : (value <$> d'))

isLeaf :: Node s a -> Bool
isLeaf Node {parent} = isNothing parent

grandestParent :: NodeRef s a -> ST s (NodeRef s a)
grandestParent rn =
  do
    n <- readSTRef rn
    case parent n of
      Just parent -> grandestParent parent
      Nothing -> pure rn

representativeElementIfMember :: Eq a => a -> [a] -> Maybe a
representativeElementIfMember x s =
  if x `elem` s
    then Just (head s)
    else Nothing

predToMaybe :: (a -> Bool) -> a -> Maybe a
predToMaybe p x = if p x then Just x else Nothing

findMap :: (a -> Maybe t) -> [a] -> Maybe t
findMap p s = asum (p <$> s)
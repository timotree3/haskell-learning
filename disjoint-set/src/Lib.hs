module Lib
  ( add,
    empty,
    union,
    find,
    DisjointSet,
  )
where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

newtype DisjointSet a = DisjointSet {unDisjointSet :: [[a]]} deriving (Show)

empty :: DisjointSet a
empty = DisjointSet []

add :: Eq a => a -> DisjointSet a -> DisjointSet a
add x d = case find x d of
  Just _ -> d
  Nothing -> DisjointSet ([x] : unDisjointSet d)

union :: Eq a => a -> a -> DisjointSet a -> DisjointSet a
union x y d =
  let (sx, d') = remove x d
   in let (sy, d'') = remove y d'
       in DisjointSet ((sx ++ sy) : unDisjointSet d'')

remove :: Eq a => a -> DisjointSet a -> ([a], DisjointSet a)
remove x (DisjointSet ss) = (fromMaybe [x] (findMap (predToMaybe (elem x)) ss), DisjointSet $ filter (notElem x) ss)

find :: Eq a => a -> DisjointSet a -> Maybe a
find x (DisjointSet ss) = findMap (representativeElementIfMember x) ss

representativeElementIfMember :: Eq a => a -> [a] -> Maybe a
representativeElementIfMember x s =
  if x `elem` s
    then Just (head s)
    else Nothing

predToMaybe :: (a -> Bool) -> a -> Maybe a
predToMaybe p x = if p x then Just x else Nothing

findMap :: (a -> Maybe t) -> [a] -> Maybe t
findMap p s = asum (p <$> s)
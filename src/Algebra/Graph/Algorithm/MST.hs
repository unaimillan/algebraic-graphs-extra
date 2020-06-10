module Algebra.Graph.Algorithm.MST where

import           Algebra.Graph
import           Data.Maybe    (listToMaybe)
import           Data.Set      (Set, toList)
import           Data.Tree

-- | Graph is assumed to be undirected and unlabelled
a, b, c, d :: Graph Int
a = 1

b = 2

c = 3

d = 4

-- Code was taken from http://stepik.org/lesson/7009/step/8
-- Version 2:
-- distribute :: Expr -> Expr
-- distribute ((e1 :+: e2) :*: e) =
--   distribute (expand e1 :*: expand e) :+: distribute (expand e2 :*: expand e)
-- distribute (e :*: (e1 :+: e2)) =
--   distribute (expand e :*: expand e1) :+: distribute (expand e :*: expand e2)
-- distribute e = e
-- expand :: Expr -> Expr
-- expand (e1 :+: e2) = expand e1 :+: expand e2
-- expand (e1 :*: e2) = distribute (expand e1 :*: expand e2)
-- expand e           = e
--
-- Version 1:
-- expand :: Graph a -> Graph a
-- expand = foldr1 Overlay . expandList
--   where
--
-- | Expand graph to list of cliques
expandList :: Graph a -> [Graph a]
expandList Empty = []
expandList (Vertex x) = [Vertex x]
expandList (Overlay x y) = expandList x ++ expandList y
expandList (Connect x y) =
  [Connect e1 e2 | e1 <- expandList x, e2 <- expandList y]

-- | Convert list of cliques to list of vertex sets
-- None: All vertexes in the clique are connected to all the other ones
toVertexSets :: Ord a => [Graph a] -> [Set a]
toVertexSets = map vertexSet

cliqueToTree :: Ord a => Set a -> Maybe (Tree a)
cliqueToTree x =
  case node of
    Nothing -> Nothing
    Just a  -> Just (Node a (map (flip Node []) leafs))
  where
    lst = toList x
    node = listToMaybe (take 1 lst)
    leafs = drop 1 lst

mergeCliques :: Ord a => [Set a] -> Maybe (Tree a)
mergeCliques lst = do
  rootSet <- listToMaybe lst
  root <- cliqueToTree rootSet
  let least = drop 1 lst
  snd <$> foldr treeMerge (Just (rootSet, root)) least
  where
    treeMerge ::
         Ord a => Set a -> Maybe (Set a, Tree a) -> Maybe (Set a, Tree a)
    treeMerge cand inp = do
      (curSet, cur) <- inp
      -- find `intersection` and `difference` of curSet and candSet, then
      -- and all different as leafs to one of intersection
      return (curSet, cur)

mst :: Ord a => Graph a -> Maybe (Tree a)
mst = mergeCliques . toVertexSets . expandList
-- [(Set a, Tree a)]

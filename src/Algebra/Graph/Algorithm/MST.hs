module Algebra.Graph.Algorithm.MST where

import           Algebra.Graph
import           Data.Set      (Set)
import           Data.Tree

a, b, c :: Graph Int
a = Vertex 1

b = Vertex 2

c = Vertex 3

-- Code is taken from http://stepik.org/lesson/7009/step/8
-- | Version 2:
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
expandList :: Graph a -> [Graph a]
expandList Empty = []
expandList (Vertex x) = [Vertex x]
expandList (Overlay x y) = expandList x ++ expandList y
expandList (Connect x y) =
  [Connect e1 e2 | e1 <- expandList x, e2 <- expandList y]

toCliqueList :: Ord a => [Graph a] -> [Set a]
toCliqueList = map vertexSet

mst :: Ord a => a -> Graph a -> Forest a
mst _ Empty         = []
mst _ (Vertex x)    = [Node x []]
mst v (Overlay a b) = mst v a <> mst v b
mst v (Connect a b) = undefined

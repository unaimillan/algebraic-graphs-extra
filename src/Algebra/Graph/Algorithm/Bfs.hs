-- |
-- Breadth-first search (BFS) for algebraic graphs and BFS-based algorithms.
module Algebra.Graph.Algorithm.Bfs where

import           Algebra.Graph
import qualified Data.Set      as S

-- * BFS core

-- | \( O(?) \).
-- BFS for algebraic graphs.
--
-- Returns list of reachable vertices with corresponding distances
-- from the starting vertex.
--
-- >>> bfs (1 * 2 + 2 * (3 * 4)) 1
-- [(2,1),(3,2),(4,2)]
bfs :: Ord a => Graph a -> a -> [(a, Int)]
bfs graph s = bfsLoop graph s initial S.empty 1
  where
    initial = bfsDepth graph s 1

-- ** Variations

-- | The body of bfs algorithm.
bfsLoop :: Ord a => Graph a -> a -> [(a, Int)] -> S.Set a -> Int -> [(a, Int)]
bfsLoop _ _ [] _ _ = []
bfsLoop graph s (x:xs) visited depth
  | (S.notMember p visited) && (s /= p) = x : bfsLoop graph s (xs <> conns) (S.insert p visited) (depth + 1)
  | otherwise = bfsLoop graph s xs visited (depth + 1)
  where
    conns = bfsDepth graph p (depth + 1)
    (p, _) = x

-- | TODO: what is this?
bfsDepth :: Ord a => Graph a -> a -> Int -> [(a, Int)]
bfsDepth graph t d = (\x -> (x, d)) <$> adjacentTo t graph

-- * Helpers

-- | \( O(s) \).
-- Find all vertices adjacent to a given one (including the vertex itself).
--
-- **NOTE:** may contain duplicates!
--
-- >>> adjecentTo 2 (1 * 2 + 2 * 3 * 4)
-- Just [2,2,3,4]
--
-- Returns empty list if starting point does not belong to the graph:
--
-- >>> adjacentTo 5 (1 * 2 + 2 * 3 * 4)
-- []
adjacentTo :: Eq a => a -> Graph a -> [a]
adjacentTo _ Empty = []
adjacentTo t (Vertex u)
  | u == t = [t]
  | otherwise = []
adjacentTo t (Connect l r)
  | null leftVertices = adjacentTo t r
  | otherwise         = leftVertices <> extractVertices r
  where
    leftVertices = adjacentTo t l
adjacentTo t (Overlay l r) = adjacentTo t l <> adjacentTo t r

-- | \( O(?) \).
-- Find all reachable vertices a given vertex.
reachableFrom :: Eq a => a -> Graph a -> [a]
reachableFrom t (Overlay l r) = error "not implemented"
reachableFrom t g             = adjacentTo t g

-- | \( O(?) \).
-- Find all reachable vertices together with distance from a given vertex.
distancesFrom :: Eq a => a -> Graph a -> [(a, Int)]
distancesFrom = error "not implemented"

-- | \( O(s) \).
-- Extract all vertices from given graph.
--
-- **NOTE:** may contain duplicates!
--
-- >>> extractVertices (1 * 2 + 2 * (3 * 4))
-- [1,2,2,3,4]
extractVertices :: Graph a -> [a]
extractVertices Empty         = []
extractVertices (Vertex x)    = [x]
extractVertices (Connect x y) = extractVertices x <> extractVertices y
extractVertices (Overlay x y) = extractVertices x <> extractVertices y

-- | TODO: remove this (it is not needed).
unwrapMaybeList :: Maybe [a] -> [a]
unwrapMaybeList Nothing  = []
unwrapMaybeList (Just x) = x

-- |
-- Breadth-first search (BFS) for directed algebraic graphs and BFS-based algorithms.
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
    initial = (\x -> (x, 1)) <$> adjacentTo s graph

-- ** Variations

-- | The body of bfs algorithm.
bfsLoop :: Ord a => Graph a -> a -> [(a, Int)] -> S.Set a -> Int -> [(a, Int)]
bfsLoop _ _ [] _ _ = []
bfsLoop graph s (x:xs) visited depth
  | (S.notMember p visited) && (s /= p) = x : bfsLoop graph s (xs <> conns) (S.insert p visited) (depth + 1)
  | otherwise = bfsLoop graph s xs visited (depth + 1)
  where
    conns = (\v -> (v, depth)) <$> adjacentTo p graph
    (p, _) = x

-- * Helpers

-- | \( O(s) \).
-- Find all vertices adjacent to a given one (including the vertex itself).
--
-- **NOTE:** may contain duplicates!
--
-- >>> adjacentTo 2 (1 * 2 + 2 * 3 * 4)
-- [2,2,3,4]
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

-- | \( O(s^2) \). Not sure.
-- Find all reachable vertices a given vertex.
--
-- **NOTE:** may contain duplicates and starting point!
--
-- >>> reachableFrom 1 (1 * 2 + 2 * 3 + 3 * 4)
-- [1,2,2,3,3,4]
reachableFrom :: Eq a => a -> Graph a -> [a]
reachableFrom t (Overlay l r) = left <> right
  where
    left = concat $ (\x -> x : reachableFrom x r) <$> reachableFrom t l
    right = reachableFrom t r
reachableFrom t g = adjacentTo t g

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

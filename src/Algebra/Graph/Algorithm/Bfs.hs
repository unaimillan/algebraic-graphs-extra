-- |
-- Breadth-first search (BFS) for directed algebraic graphs and BFS-based algorithms.
module Algebra.Graph.Algorithm.Bfs where

import           Algebra.Graph
import           Data.Set      (Set)
import qualified Data.Set      as Set

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
bfs graph s = bfsLoop graph s initial Set.empty 1
  where
    initial = (\x -> (x, 1)) <$> adjacentTo s graph

-- ** Variations

-- | The body of bfs algorithm.
bfsLoop :: Ord a => Graph a -> a -> [(a, Int)] -> Set a -> Int -> [(a, Int)]
bfsLoop _ _ [] _ _ = []
bfsLoop graph s (x:xs) visited depth
  | (Set.notMember p visited) && (s /= p) = x : bfsLoop graph s (xs <> conns) (Set.insert p visited) (depth + 1)
  | otherwise = bfsLoop graph s xs visited (depth + 1)
  where
    conns = (\v -> (v, depth)) <$> adjacentTo p graph
    (p, _) = x

-- * Helpers

-- | \( O(s) \).
-- Find all vertices adjacent to a given one.
--
-- >>> adjacentTo 2 (1 * 2 + 2 * 3 * 4)
-- [3,4]
--
-- NOTE: list might contain duplicates:
--
-- >>> adjacentTo 1 ((1 + 2) * (3 + 4) + (1 + 3) * (4 + 5))
-- [3,4,4,5]
--
-- Returns empty list if starting point does not belong to the graph:
--
-- >>> adjacentTo 5 (1 * 2 + 2 * 3 * 4)
-- []
adjacentTo :: Eq a => a -> Graph a -> [a]
adjacentTo t = concat . go
  where
    go Empty = Nothing
    go (Vertex u)
      | u == t = Just []
      | otherwise = Nothing
    go (Connect l r) =
      case go l of
        Nothing           -> go r
        Just leftVertices -> Just (leftVertices <> extractVertices r)
    go (Overlay l r) = go l <> go r

-- | \( O(s) \).
-- Find all vertices adjacent to a given one.
--
-- Like 'adjacentTo', but guarantees no duplicates:
--
-- >>> adjacentToSet 1 ((1 + 2) * (3 + 4) + (1 + 3) * (2 + 4))
-- fromList [2,3,4]
adjacentToSet :: Ord a => a -> Graph a -> Set a
adjacentToSet t = Set.fromList . adjacentTo t

-- | \( O(s*n) \). Not sure.
-- Find all reachable vertices a given vertex.
--
-- **NOTE:** may contain duplicates and starting point!
--
-- >>> reachableFrom 1 (1 * 2 + 2 * 3 + 3 * 4)
-- [2,3,4]
reachableFrom :: Eq a => a -> Graph a -> [a]
reachableFrom t (Overlay l r) = left <> right
  where
    left = concat $ (\x -> x : reachableFrom x r) <$> reachableFrom t l
    right = reachableFrom t r
reachableFrom t g = adjacentTo t g

-- | \( O(s*n) \). Not sure.
-- Find all reachable vertices together with distance from a given vertex.
--
-- >>> distancesFrom 1 (1 * 2 + 2 * 3 + 3 * 4)
-- [(2,1),(3,2),(4,3)]
distancesFrom :: Eq a => a -> Graph a -> [(a, Int)]
distancesFrom t g = distancesFromHelper t 1 g

distancesFromHelper :: Eq a => a -> Int -> Graph a -> [(a, Int)]
distancesFromHelper t d (Overlay l r) = left <> right
  where
    left = concat
      $ (\(x, y) -> (x, y) : distancesFromHelper x (y + 1) r)
      <$> distancesFromHelper t d l
    right = distancesFromHelper t d r
distancesFromHelper t d g = (\x -> (x, d)) <$> adjacentTo t g

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


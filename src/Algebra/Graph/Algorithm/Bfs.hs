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

-- | \( O(s*n) \). Not sure.
-- Find all reachable vertices a given vertex.
--
-- **NOTE:** may contain duplicates and starting point!
--
-- >>> reachableFrom 1 (1 * 2 + 2 * 3 + 3 * 4)
-- [1,2,3,4]
--
-- >>> reachableFrom 1 ((1 * 2 + 3 * 4) + (2 * 3))
-- [1,2,3,3,4]
reachableFrom :: Eq a => a -> Graph a -> [a]
reachableFrom t g = t : reachableFromHelper t Empty g

reachableFromHelper :: Eq a => a -> Graph a -> Graph a -> [a]
reachableFromHelper t Empty (Overlay l r) = left <> right
  where
    left = concat $ (\x -> x : reachableFromHelper x Empty r) <$> reachableFromHelper t r l
    right = reachableFromHelper t Empty r
reachableFromHelper t rightSide (Overlay l r) = left <> right
  where
    rightSideCheck = concat $ (\x -> x : reachableFromHelper x Empty rightSide) <$> reachableFromHelper t rightSide l
    left = concat $ (\x -> x : reachableFromHelper x Empty r) <$> rightSideCheck
    right = reachableFromHelper t Empty r
reachableFromHelper t _ g = adjacentTo' t g

-- | \( O(s*n) \). Not sure.
-- Find all reachable vertices together with distance from a given vertex.
--
-- **NOTE:** may contain duplicates and starting point!
--
-- >>> distancesFrom 1 (1 * 2 + 2 * 3 + 3 * 4)
-- [(1,0),(2,1),(3,2),(4,3)]
--
-- >>> distancesFrom 1 ((1 * 2 + 3 * 4) + (2 * 3))
-- [(1,0),(2,1),(3,2),(3,2),(4,3)]
distancesFrom :: Eq a => a -> Graph a -> [(a, Int)]
distancesFrom t g = (t, 0) : distancesFromHelper t 1 Empty g

distancesFromHelper :: Eq a => a -> Int -> Graph a -> Graph a -> [(a, Int)]
distancesFromHelper t d Empty (Overlay l r) = left <> right
  where
    left = concat
      $ (\(x, y) -> (x, y) : distancesFromHelper x (y + 1) Empty r)
      <$> distancesFromHelper t d r l
    right = distancesFromHelper t d Empty r
distancesFromHelper t d rightSide (Overlay l r) = left <> right
  where
    rightSideCheck = concat
      $ (\(x, y) -> (x, y) : distancesFromHelper x (y + 1) Empty rightSide)
      <$> distancesFromHelper t d r l
    left = concat
      $ (\(x, y) -> (x, y) : distancesFromHelper x (y + 1) Empty r)
      <$> rightSideCheck
    right = distancesFromHelper t d Empty r
distancesFromHelper t d _ g = (\x -> (x, d)) <$> adjacentTo' t g

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

-- * Temporary functions

-- | \( O(s) \).
-- Find all vertices adjacent to a given one).
--
-- >>> adjacentTo' 2 (1 * 2 + 2 * 3 * 4)
-- [3,4]
--
-- Returns empty list if starting point does not belong to the graph:
--
-- >>> adjacentTo' 5 (1 * 2 + 2 * 3 * 4)
-- []
-- >>> adjacentTo' 1 ((1 + 2) * (3 + 4) + (1 + 3) * (2 + 4))
-- [3,4,2,4]
adjacentTo' :: Eq a => a -> Graph a -> [a]
adjacentTo' t g = unwrapMaybeList $ adjacentToHelper t g

adjacentToHelper :: Eq a => a -> Graph a -> Maybe [a]
adjacentToHelper _ Empty = Nothing
adjacentToHelper t (Vertex x)
  | t == x = Just []
  | otherwise = Nothing
adjacentToHelper t (Connect (Vertex x) (Vertex y))
  | x == t = Just [y]
  | y == t = Just []
  | otherwise = Nothing
adjacentToHelper t (Connect (Vertex x) y)
  | x == t = Just (extractVertices y)
  | otherwise = adjacentToHelper t y
adjacentToHelper t (Connect x y) = Just (left <> right)
  where
    left = unwrapMaybeList $ (<>) <$> (adjacentToHelper t x) <*> Just (extractVertices y)
    right = unwrapMaybeList $ adjacentToHelper t y
adjacentToHelper t (Overlay x y) = (adjacentToHelper t x) <> (adjacentToHelper t y)

unwrapMaybeList :: Maybe [a] -> [a]
unwrapMaybeList Nothing = []
unwrapMaybeList (Just x) = x

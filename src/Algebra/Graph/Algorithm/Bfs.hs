module Algebra.Graph.Algorithm.Bfs(bfs) where

import           Algebra.Graph
import qualified Data.Set      as S

-- | /O(?)/.
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
bfsDepth graph t d = (\x -> (x, d)) <$> (unwrapMaybeList $ connectedWith graph t)

-- | /O(s)/.
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

-- | Find all vertices reachable from a given one.
--
-- >>> connectedWith (1 * 2 + 2 * (3 * 4)) 2
-- Just [3,4]
--
-- Returns 'Nothing' if starting point does not belong to the graph:
--
-- >>> connectedWith (1 * 2 + 2 * (3 * 4)) 5
-- Nothing
connectedWith :: Ord a => Graph a -> a -> Maybe [a]
connectedWith Empty _ = Nothing
connectedWith (Vertex _) _ = Nothing
connectedWith (Connect (Vertex x) (Vertex y)) t
  | x == t = Just [y]
  | y == t = Just []
  | otherwise = Nothing
connectedWith (Connect (Vertex x) y) t
  | x == t = Just (extractVertices y)
  | otherwise = connectedWith y t
connectedWith (Connect x y) t = Just (left <> right)
  where
    left = unwrapMaybeList $ (<>) <$> (connectedWith x t) <*> Just (extractVertices y)
    right = unwrapMaybeList $ connectedWith y t
connectedWith (Overlay x y) t = (connectedWith x t) <> (connectedWith y t)

-- | TODO: remove this (it is not needed).
unwrapMaybeList :: Maybe [a] -> [a]
unwrapMaybeList Nothing  = []
unwrapMaybeList (Just x) = x



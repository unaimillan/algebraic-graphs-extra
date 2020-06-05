module Algebra.Graph.Algorithm.Bfs(bfs) where

import Algebra.Graph
import qualified Data.Set as S

-- Extracts all vertices values from given graph
extractVertices :: Graph a -> [a]
extractVertices Empty = []
extractVertices (Vertex x) = [x]
extractVertices (Connect x y) = (extractVertices x) <> (extractVertices y)
extractVertices (Overlay x y) = (extractVertices x) <> (extractVertices y)

-- Finds all vertices which connected with given point.
-- Returns Nothing if point wasn't found in the graph.
-- Returns Just [a] (where [a] is list of points) otherwise.
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

unwrapMaybeList :: Maybe [a] -> [a]
unwrapMaybeList Nothing = []
unwrapMaybeList (Just x) = x

bfsDepth :: Ord a => Graph a -> a -> Int -> [(a, Int)]
bfsDepth graph t d = (\x -> (x, d)) <$> (unwrapMaybeList $ connectedWith graph t)

-- The body of bfs algorithm
bfsLoop :: Ord a => Graph a -> a -> [(a, Int)] -> S.Set a -> Int -> [(a, Int)]
bfsLoop _ _ [] _ _ = []
bfsLoop graph s (x:xs) visited depth
  | (S.notMember p visited) && (s /= p) = x : bfsLoop graph s (xs <> conns) (S.insert p visited) (depth + 1)
  | otherwise = bfsLoop graph s xs visited (depth + 1)
  where
    conns = bfsDepth graph p (depth + 1)
    (p, _) = x

-- The main Breadth-first search function
bfs :: Ord a => Graph a -> a -> [(a, Int)]
bfs graph s = bfsLoop graph s initial S.empty 1
  where
    initial = bfsDepth graph s 1

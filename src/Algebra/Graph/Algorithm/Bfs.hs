module Algebra.Graph.Algorithm.Bfs(bfs) where

import Algebra.Graph

extractVertices :: Graph a -> [a]
extractVertices Empty = []
extractVertices (Vertex x) = [x]
extractVertices (Connect x y) = (extractVertices x) <> (extractVertices y)
extractVertices (Overlay x y) = (extractVertices x) <> (extractVertices y)

connectedWithHelper :: Eq a => Graph a -> a -> Maybe [a]
connectedWithHelper Empty _ = Nothing
connectedWithHelper (Vertex _) _ = Nothing
connectedWithHelper (Connect (Vertex x) (Vertex y)) t
  | x == t = Just [y]
  | y == t = Just []
  | otherwise = Nothing
connectedWithHelper (Connect (Vertex x) y) t
  | x == t = Just (extractVertices y)
  | otherwise = connectedWithHelper y t
connectedWithHelper (Connect x y) t = Just (left <> right)
  where
    left = unwrapMaybeList $ (<>) <$> (connectedWithHelper x t) <*> Just (extractVertices y)
    right = unwrapMaybeList $ connectedWithHelper y t
connectedWithHelper (Overlay x y) t = (connectedWithHelper x t) <> (connectedWithHelper y t)

connectedWith :: Eq a => Graph a -> a -> [a]
connectedWith graph t = unwrapMaybeList $ connectedWithHelper graph t

unwrapMaybeList :: Maybe [a] -> [a]
unwrapMaybeList Nothing = []
unwrapMaybeList (Just x) = x

bfsDepth :: Eq a => Graph a -> a -> Int -> [(a, Int)]
bfsDepth graph t d = (\x -> (x, d)) <$> (connectedWith graph t)

bfsLoop :: Eq a => Graph a -> a -> [(a, Int)] -> [a] -> Int -> [(a, Int)]
bfsLoop _ _ [] _ _ = []
bfsLoop graph s (x:xs) visited depth
  | (notElem p visited) && (s /= p) = x : bfsLoop graph s (xs <> conns) (p:visited) (depth + 1)
  | otherwise = bfsLoop graph s xs (p:visited) (depth + 1)
  where
    conns = bfsDepth graph p (depth + 1)
    (p, _) = x

bfs :: Eq a => Graph a -> a -> [(a, Int)]
bfs graph s = bfsLoop graph s initial [] 1
  where
    initial = bfsDepth graph s 1

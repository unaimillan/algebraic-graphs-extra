module Algebra.Graph.Algorithm where

import Algebra.Graph

shortestPath :: Eq a => Graph a -> a -> a -> Maybe (Int, [a])

shortestPath Empty _ _ = Nothing

shortestPath (Vertex x) _ t
  | x == t = Just (1, [x])
  | otherwise = Nothing

shortestPath (Connect x y) s t =
  case (path_x, path_y) of
    (Just (n1, p1), Just (n2, p2)) ->
      Just (connHelper (n1, p1) (n2, p2))
    (Just (n1, p1), Nothing) ->
      Just (n1+1, p1)
    (Nothing, Just (n2, p2)) ->
      Just (n2+1, p2)
    _ ->
      Nothing
  where
    path_x = shortestPath x s t
    path_y = shortestPath y s t

shortestPath (Overlay x y) s t =
  case (path_x, path_y) of
    (Just (n1, p1), Just (n2, p2)) ->
      Just (connHelper (n1, p1) (n2, p2))
    (Just (n1, p1), Nothing) ->
      Just (n1+1, p1)
    (Nothing, Just (n2, p2)) ->
      Just (n2+1, p2)
    _ ->
      Nothing
  where
    path_x = shortestPath x s t
    path_y = shortestPath y s t

connHelper :: (Int, [a]) -> (Int, [a]) -> (Int, [a])
connHelper (n1, p1) (n2, p2)
  | n1 > n2 = (n1 + 1, p1)
  | otherwise = (n2 + 1, p2)

findConnect :: Eq a => Graph a -> a -> [Graph a]
findConnect Empty _ = []
findConnect (Vertex _) _ = []
findConnect (Connect (Vertex x) y) t
  | x == t = (Connect (Vertex x) y) : findConnect y t
  | otherwise = findConnect y t
findConnect (Connect x y) t = (findConnect x t) <> (findConnect y t)
findConnect (Overlay x y) t = (findConnect x t) <> (findConnect y t)

extractVertices :: Graph a -> [a]
extractVertices Empty = []
extractVertices (Vertex x) = [x]
extractVertices (Connect x y) = (extractVertices x) <> (extractVertices y)
extractVertices (Overlay x y) = (extractVertices x) <> (extractVertices y)

connectedWith :: Eq a => Graph a -> a -> [a]
connectedWith Empty _ = []
connectedWith (Vertex _) _ = []
connectedWith (Connect (Vertex x) y) t
  | x == t = (extractVertices y)
  | otherwise = connectedWith y t
connectedWith (Connect x y) t = (connectedWith x t) <> (connectedWith y t)
connectedWith (Overlay x y) t = (connectedWith x t) <> (connectedWith y t)

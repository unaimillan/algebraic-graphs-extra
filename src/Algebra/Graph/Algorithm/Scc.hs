module Algebra.Graph.Algorithm.Scc where

import           Algebra.Graph
import           Algebra.Graph.Algorithm.Internal
import           Data.Maybe
import qualified Data.Set                         as Set

-- | O(n^2 * s)
-- Groups elements into graph of a graph by rule (first function)
--
-- >>> let g = (1 + 2) * (3 + 4)
-- >>> let f x y = even (x + y)
-- >>> groupBy f g
-- Overlay (Vertex (Connect (Vertex 1) (Vertex 3))) (Vertex (Connect (Vertex 2) (Vertex 4)))
groupBy :: Ord a => (a -> a -> Bool) -> Graph a -> Graph (Graph a)
groupBy f g = foldl1
  (\x y -> if isAdjacentGraphs x y then connect x y else overlay x y)
  $ map (vertex . (`constructGroup` g))
  $ groupToList f g

-- | O(n^2)
-- Groups elements into list of lists by rule (first function)
--
-- >>> let g = (1 + 2) * (3 + 4)
-- >>> let f x y = even (x + y)
-- >>> groupToList f g
-- [[1,3],[2,4]]
groupToList :: Ord a => (a -> a -> Bool) -> Graph a -> [[a]]
groupToList f g = filter (not . null) $ do
  x <- list
  y <- list
  return $ if f x y && x /= y && x < y then [x, y] else []
  where
    list = extractVertices g

-- | O(n^2 * s)
--
-- >>> constructGroup [1,3] ((1 + 2) * (3 + 4))
-- Connect (Vertex 1) (Vertex 3)
constructGroup :: Ord a => [a] -> Graph a -> Graph a
constructGroup list g = overlays $ map conn $ filter isJust $ do
  x <- list
  y <- list
  return $ if x /= y && hasEdge x y g then
    Just (x, y) else Nothing
    where
      conn (Just (x, y)) = edge x y
      conn Nothing       = Empty

-- | O(s * log n)
-- Checks either right graph achievable from left one or not.
--
-- >>> isAdjacentGraphs ((1 * 2) + (2 * 3)) (2 * 4)
-- True
-- >>> isAdjacentGraphs ((1 * 2) + (2 * 3)) (1 * 4)
-- False
isAdjacentGraphs :: Ord a => Graph a -> Graph a -> Bool
isAdjacentGraphs l r = not $ Set.disjoint (left l) right
  where
    left Empty           = Set.empty
    left (Vertex v)      = Set.singleton v
    left (Connect _ r')  = vertexSet r'
    left (Overlay l' r') = Set.union (left l') (left r')
    right = vertexSet r

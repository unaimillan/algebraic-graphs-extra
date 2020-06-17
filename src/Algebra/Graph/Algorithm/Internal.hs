module Algebra.Graph.Algorithm.Internal where

import                            Algebra.Graph
import Data.DisjointSet           (DisjointSet)
import qualified Data.DisjointSet as DS

-- | Extract disjoint set of connectivity components from the graph
--
-- >>> DS.pretty $ components ((1 * 2) + (3 * 4))
-- "{{1,2},{3,4}}"
--
-- >>> DS.pretty $ components (((1 * 2) + (3 * 4)) + (2 * 3))
-- "{{1,2,3,4}}"
components :: Ord a => Graph a -> DisjointSet a
components Empty         = DS.empty
components (Vertex x)    = DS.singleton x
components (Connect x y) = DS.fromLists [extractVertices x <> extractVertices y]
components (Overlay x y) = (components x) <> (components y)

-- | \( O(s) \).
--
-- This function extracts all vertices from a given graph.
-- It is alternative version of VertexList function
-- but it does not care about duplicates and has better complexity.
--
-- >>> extractVertices ((1 * 2) + (3 * 4))
-- [1,2,3,4]
extractVertices :: Graph a -> [a]
extractVertices Empty = []
extractVertices (Vertex v) = [v]
extractVertices (Connect l r) = left <> right
  where
    left = extractVertices l
    right = extractVertices r
extractVertices (Overlay l r) = left <> right
  where
    left = extractVertices l
    right = extractVertices r

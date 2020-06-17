module Algebra.Graph.Algorithm.Internal where

import                            Algebra.Graph
import Data.DisjointSet           (DisjointSet)
import qualified Data.DisjointSet as DisjointSet

-- | Extract disjoint set of connectivity components from the graph
--
-- >>> DisjointSet.pretty $ components ((1 * 2) + (3 * 4))
-- "{{1,2},{3,4}}"
--
-- >>> DisjointSet.pretty $ components (((1 * 2) + (3 * 4)) + (2 * 3))
-- "{{1,2,3,4}}"
components :: Ord a => Graph a -> DisjointSet a
components Empty           = DisjointSet.empty
components (Vertex x)      = DisjointSet.singleton x
components g@(Connect _ _) = DisjointSet.singletons $ vertexSet g
components (Overlay x y)   = components x <> components y

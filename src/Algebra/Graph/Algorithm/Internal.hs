module Algebra.Graph.Algorithm.Internal where

import           Algebra.Graph
import qualified Data.DisjointSet as DS
import qualified Data.Set         as S

-- | Extract disjoint set of connectivity components from the graph
components :: Ord a => Graph a -> DS.DisjointSet a
components Empty         = DS.empty
components (Vertex x)    = DS.singleton x
components (Overlay x y) = mergeComponents (components x) (components y)
components (Connect x y) = DS.fromLists [vertexList x ++ vertexList y]

-- | Merge two disjoint sets
mergeComponents ::
     Ord a => DS.DisjointSet a -> DS.DisjointSet a -> DS.DisjointSet a
mergeComponents x y = foldr insertComponent x (DS.toSets y)

-- | Insert a set into the other disjoint set structure:
-- Firstly, we find all intersections between the given set `x` and any of
-- disjoint ones. Then we unite all the intersecting sets into one big union
-- and finally append this big union to all other sets, with which we were not
-- intersected.
insertComponent :: Ord a => S.Set a -> DS.DisjointSet a -> DS.DisjointSet a
insertComponent x dsu =
  DS.fromLists (S.toList (S.unions (x : connected)) : disconnected)
  where
    sets = DS.toSets dsu
    connected = filter (not . S.disjoint x) sets
    disconnected = map S.toList (filter (S.disjoint x) sets)

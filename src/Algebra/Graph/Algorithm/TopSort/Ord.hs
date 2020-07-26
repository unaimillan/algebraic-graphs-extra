module Algebra.Graph.Algorithm.TopSort.Ord where

import           Algebra.Graph
import           Control.Monad.State.Lazy
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set

-- | O(s * log n)
--
-- Calculates topological number for each vertex.
--
-- >>> g = (1 * 2) + (2 * 3)
-- >>> g_final = overlay (edge (0, 1) (1, 2)) (edge (1, 2) (2, 3))
-- >>> topSort g == Just g_final
-- True
--
-- If graph contains cycle, returns Nothing.
--
-- >>> g = (1 * 2) + (2 * 3) + (3 * 1)
-- >>> topSort g
-- Nothing
topSort :: Ord a => Graph a -> Maybe (Graph (Int, a))
topSort g = if isAcyclic g then Just $ evalState (traverseGraph g) s
                                else Nothing
  where
    s = TopSortState 0 Map.empty

data TopSortState a = TopSortState
  { nextIndex     :: Int
  , vertexIndices :: Map a Int
  }

traverseGraph :: Ord a => Graph a -> State (TopSortState a) (Graph (Int, a))
traverseGraph Empty = return Empty
traverseGraph (Vertex v) = do
  TopSortState n_i vs <- get
  case Map.lookup v vs of
    Just i -> do
      return $ Vertex (i, v)
    Nothing -> do
      put $ TopSortState (n_i + 1) (Map.insert v n_i vs)
      return $ Vertex (n_i, v)
traverseGraph (Overlay l r) = do
  left <- traverseGraph l
  right <- traverseGraph r
  return $ Overlay left right
traverseGraph (Connect l r) = do
  left <- traverseGraph l
  right <- traverseGraph r
  return $ Connect left right

-- | O(s * log n)
--
-- Checks whether graph is acyclic or not.
--
-- >>> isAcyclic ((1 * 2) + (2 * 3))
-- True
--
-- >>> isAcyclic (((1 * 2) + (2 * 3)) + (3 * 1))
-- False
isAcyclic :: Ord a => Graph a -> Bool
isAcyclic Empty         = True
isAcyclic (Vertex _)    = True
isAcyclic (Overlay l r) =
  isAcyclic l
  && isAcyclic r
  && Set.disjoint left right
  where
    left = if not (connectExist l) then Set.empty else leftVertices l
    right = if not (connectExist r) then Set.empty else rightVertices r
isAcyclic (Connect l r) = isAcyclic l && isAcyclic r
  && Set.disjoint (vertexSet l) (vertexSet r)

rightVertices :: Ord a => Graph a -> Set a
rightVertices Empty         = Set.empty
rightVertices (Vertex v)    = Set.singleton v
rightVertices (Overlay l r) = Set.union (rightVertices l) (rightVertices r)
rightVertices (Connect _ r) = rightVertices r

leftVertices :: Ord a => Graph a -> Set a
leftVertices Empty         = Set.empty
leftVertices (Vertex v)    = Set.singleton v
leftVertices (Overlay l r) = Set.union (leftVertices l) (leftVertices r)
leftVertices (Connect l _) = leftVertices l

connectExist :: Graph a -> Bool
connectExist Empty         = False
connectExist (Vertex _)    = False
connectExist (Connect _ _) = True
connectExist (Overlay l r) = connectExist l || connectExist r

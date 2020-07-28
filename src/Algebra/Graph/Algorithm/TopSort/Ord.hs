module Algebra.Graph.Algorithm.TopSort.Ord where

import           Algebra.Graph
import           Algebra.Graph.Algorithm.TopNum.Ord (isAcyclic)
import           Control.Monad.State.Lazy
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map

-- | O(s * log n)
--
-- Calculates topological number for each vertex.
--
-- *NOTE*: All indices are unique.
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
topSort g
  | isAcyclic g = Just $ evalState (traverseGraph g) initialTopSortState
  | otherwise = Nothing
  where
    initialTopSortState = TopSortState 0 Map.empty

data TopSortState a = TopSortState
  { nextIndex     :: Int
  , vertexIndices :: Map a Int
  }

traverseGraph :: Ord a => Graph a -> State (TopSortState a) (Graph (Int, a))
traverseGraph Empty = return Empty
traverseGraph (Vertex v) = do
  TopSortState n_i vs <- get
  case Map.lookup v vs of
    Just i ->
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

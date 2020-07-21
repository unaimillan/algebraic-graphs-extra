module Algebra.Graph.Algorithm.TopSort.Ord where

import           Algebra.Graph
import           Control.Monad.State.Lazy
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map

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
topSort g = evalState (traverseGraph g) s
  where
    s = TopSortState 0 Map.empty False

data TopSortState a = TopSortState
  { nextIndex      :: Int
  , vertexIndices  :: Map a Int
  , dublicateFound :: Bool
  }

traverseGraph :: Ord a => Graph a -> State (TopSortState a) (Maybe (Graph (Int, a)))
traverseGraph Empty = return $ Just Empty
traverseGraph (Vertex v) = do
  TopSortState n_i vs d <- get
  case Map.lookup v vs of
    Just i -> do
      put $ TopSortState n_i vs True
      return $ Just $ Vertex (i, v)
    Nothing -> do
      put $ TopSortState (n_i + 1) (Map.insert v n_i vs) d
      return $ Just $ Vertex (n_i, v)
traverseGraph (Overlay l r) = do
  left <- traverseGraph l
  right <- traverseGraph r
  return $ do
    l' <- left
    Overlay l' <$> right
traverseGraph (Connect l r) = do
  left <- traverseGraph l
  modify (\s -> s {dublicateFound = False})
  right <- traverseGraph r
  s <- get
  return $ if dublicateFound s then Nothing else do
    l' <- left
    Connect l' <$> right

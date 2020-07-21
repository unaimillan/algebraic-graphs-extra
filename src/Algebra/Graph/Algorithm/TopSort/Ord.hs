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
topSort g = evalState (traverseGraph g) (0, Map.empty, False)

traverseGraph :: Ord a => Graph a -> State (Int, Map a Int, Bool) (Maybe (Graph (Int, a)))
traverseGraph Empty = return $ Just Empty
traverseGraph (Vertex v) = do
  (max_i, is, b) <- get
  case Map.lookup v is of
    Just i -> do
      put (max_i, is, True)
      return $ Just $ Vertex (i, v)
    Nothing -> do
      put (max_i + 1, Map.insert v max_i is, b)
      return $ Just $ Vertex (max_i, v)
traverseGraph (Overlay l r) = do
  left <- traverseGraph l
  right <- traverseGraph r
  return $ do
    l' <- left
    Overlay l' <$> right
traverseGraph (Connect l r) = do
  left <- traverseGraph l
  modify (\(x, y, _) -> (x, y, False))
  right <- traverseGraph r
  (_, _, b) <- get
  return $ if b then Nothing else do
    l' <- left
    Connect l' <$> right

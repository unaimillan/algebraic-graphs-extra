module Algebra.Graph.Algorithm.TopSort.Ord where

import           Algebra.Graph
import           Control.Monad.State.Lazy
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map

-- | O(s * log n)
--
-- TODO: "cycles checking"
--
-- >>> g = (1 * 2) + (2 * 3)
-- >>> topSort g == overlay (edge (0, 1) (1, 2)) (edge (1, 2) (2, 3))
-- True
topSort :: Ord a => Graph a -> Graph (Int, a)
topSort g = evalState (traverseGraph g) (0, Map.empty)

traverseGraph :: Ord a => Graph a -> State (Int, Map a Int) (Graph (Int, a))
traverseGraph Empty = return Empty
traverseGraph (Vertex v) = do
  (max_i, is) <- get
  case Map.lookup v is of
    Just i ->
      return $ Vertex (i, v)
    Nothing -> do
      put (max_i + 1, Map.insert v max_i is)
      return $ Vertex (max_i, v)
traverseGraph (Overlay l r) = do
  left <- traverseGraph l
  right <- traverseGraph r
  return $ Overlay left right
traverseGraph (Connect l r) = do
  left <- traverseGraph l
  right <- traverseGraph r
  return $ Connect left right

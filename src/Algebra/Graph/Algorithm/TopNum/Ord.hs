module Algebra.Graph.Algorithm.TopNum.Ord where

import           Algebra.Graph
import           Control.Monad.State.Lazy
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Maybe

-- | O(s * log n)
--
-- Calculates topological number for each vertex.
--
-- *NOTE*: Indices can repeat.
--
-- >>> g = (1 * 2) + (2 * 3)
-- >>> g_final = overlay (edge (0, 1) (1, 2)) (edge (1, 2) (2, 3))
-- >>> topNum g == Just g_final
-- True
--
-- If graph contains cycle, returns Nothing.
--
-- >>> g = (1 * 2) + (2 * 3) + (3 * 1)
-- >>> topNum g
-- Nothing
topNum :: Ord a => Graph a -> Maybe (Graph (Int, a))
topNum g = evalState (traverseGraph g) initialTopNumState
  where
    initialTopNumState = TopNumState 0 Map.empty

data TopNumState a = TopNumState
  { nextIndex     :: Int
  , vertexIndices :: Map a Int
  }

traverseGraph :: Ord a => Graph a -> State (TopNumState a) (Maybe (Graph (Int, a)))
traverseGraph Empty = return $ Just Empty
traverseGraph (Vertex v) = do
  TopNumState n_i vs <- get
  case Map.lookup v vs of
    Just i ->
      if i < n_i then
        return Nothing
      else do
        modify (\s -> s { nextIndex = i + 1 })
        return $ Just $ Vertex (i, v)
    Nothing -> do
      put $ TopNumState (n_i + 1) (Map.insert v n_i vs)
      return $ Just $ Vertex (n_i, v)
traverseGraph (Overlay l r) = do
  savedNextIndex <- gets nextIndex
  left <- traverseGraph l
  leftNextIndex <- gets nextIndex
  modify (\s -> s { nextIndex = savedNextIndex })
  right <- traverseGraph r
  rightNextIndex <- gets nextIndex
  modify (\s -> s { nextIndex = max leftNextIndex rightNextIndex })
  return $ do
    l' <- left
    Overlay l' <$> right
traverseGraph (Connect l r) = do
  left <- traverseGraph l
  right <- traverseGraph r
  return $ do
    l' <- left
    Connect l' <$> right

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
isAcyclic = isJust . topNum

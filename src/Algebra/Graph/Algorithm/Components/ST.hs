module Algebra.Graph.Algorithm.Components.ST where

import           Algebra.Graph
import           Algebra.Graph.Algorithm.Internal
import           Control.Monad
import           Control.Monad.ST
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.UnionFind.ST                (Point)
import qualified Data.UnionFind.ST                as UF

-- | O(s + n log n).
-- Extract connectivity components from the graph.
--
-- >>> components ((1 * 2) + (3 * 4))
-- [[2,1],[4,3]]
--
-- >>> components (((1 * 2) + (3 * 4)) + (2 * 3))
-- [[4,3,2,1]]
components :: Ord a => Graph a -> [[a]]
components g = runST $ do
  (ps, g') <- mkVertexPoints g Map.empty
  componentsST g'
  comp <- componentsFromPoints $ Map.toList ps
  return $ Map.elems comp

-- | O(n log n).
componentsFromPoints :: Ord a => [(a, Point s a)] -> ST s (Map a [a])
componentsFromPoints vs = do
  list <- pointsList vs
  return $ Map.fromListWith (<>) list

-- | O(n).
pointsList :: [(a, Point s a)] -> ST s [(a, [a])]
pointsList = mapM $ \(x, px) -> do
  repr <- UF.descriptor px
  return (repr, [x])

-- | O(s + n log n).
mkVertexPoints :: Ord a => Graph a -> Map a (Point s a) -> ST s (Map a (Point s a), Graph (Point s a))
mkVertexPoints Empty _ = return (Map.empty, Empty)
mkVertexPoints (Vertex v) ps = do
  case Map.lookup v ps of
    Nothing -> do
      p <- UF.fresh v
      return (Map.insert v p ps, Vertex p)
    (Just p) -> return (ps, Vertex p)
mkVertexPoints (Connect l r) ps = do
  (lp, lg) <- mkVertexPoints l ps
  (rp, rg) <- mkVertexPoints r lp
  return (rp, Connect lg rg)
mkVertexPoints (Overlay l r) ps = do
  (lp, lg) <- mkVertexPoints l ps
  (rp, rg) <- mkVertexPoints r lp
  return (rp, Overlay lg rg)

-- | O(s + n).
componentsST :: Ord a => Graph (Point s a) -> ST s ()
componentsST (Overlay l r) = componentsST l >> componentsST r
componentsST g@(Connect _ _) = do
  case extractVertices g of
    (p:ps) ->
      foldM_ (\x y -> (UF.union x y) >> (return y)) p ps
    _ -> return ()
componentsST _ = return ()

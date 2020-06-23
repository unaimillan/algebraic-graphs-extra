module Algebra.Graph.Algorithm.Internal where

import           Algebra.Graph
import           Control.Monad
import           Control.Monad.ST
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as Map
import           Data.UnionFind.ST (Point)
import qualified Data.UnionFind.ST as UF

-- | O(s + n^2)
-- Extract disjoint set of connectivity components from the graph
--
-- >>> components (((1 * 2) + (3 * 4)) + (2 * 3))
--
components :: Ord a => Graph a -> Map a [a]
components g = runST $ do
  (px, g') <- mkVertexPoints g
  componentsST g'
  componentsFromPoints px

-- | O(n log n). Not sure.
componentsFromPoints :: Ord a => [(a, Point s a)] -> ST s (Map a [a])
componentsFromPoints vs = do
  list <- pointsList vs
  return $ Map.fromListWith (<>) list

-- | O(n).
pointsList :: [(a, Point s a)] -> ST s [(a, [a])]
pointsList = mapM $ \(x, px) -> do
  repr <- UF.descriptor px
  return (repr, [x])

-- | O(s).
mkVertexPoints :: Eq a => Graph a -> ST s ([(a, Point s a)], (Graph (Point s a)))
mkVertexPoints Empty = return ([], Empty)
mkVertexPoints (Vertex v) = do
  point <- UF.fresh v
  return ([(v, point)], Vertex point)
mkVertexPoints (Connect l r) = do
  (lp, lg) <- mkVertexPoints l
  (rp, rg) <- mkVertexPoints r
  return (lp <> rp, Connect lg rg)
mkVertexPoints (Overlay l r) = do
  (lp, lg) <- mkVertexPoints l
  (rp, rg) <- mkVertexPoints r
  return (lp <> rp, Overlay lg rg)

-- | O(s + n^2). Not sure.
componentsST :: Ord a => Graph (Point s a) -> ST s ()
componentsST (Overlay l r) = componentsST l >> componentsST r
componentsST (Connect l r) = do
  left <- extractPointsST l
  right <- extractPointsST r
  sequence_ $ liftM2 UF.union left right
componentsST _ = return ()

-- | O(s).
extractPointsST :: Graph (Point s a) -> ST s [Point s a]
extractPointsST Empty = return []
extractPointsST (Vertex v) = return [v]
extractPointsST (Connect l r) = do
  left <- extractPointsST l
  right <- extractPointsST r
  return $ left <> right
extractPointsST (Overlay l r) = do
  left <- extractPointsST l
  right <- extractPointsST r
  return $ left <> right

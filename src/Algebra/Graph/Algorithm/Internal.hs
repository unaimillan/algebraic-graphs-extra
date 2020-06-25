{-# LANGUAGE FlexibleContexts #-}
module Algebra.Graph.Algorithm.Internal where

import           Algebra.Graph
import           Control.Monad
import           Control.Monad.ST
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as Map
import           Data.UnionFind.ST (Point)
import qualified Data.UnionFind.ST as UF

-- | O(s + n log n)
-- Extract disjoint set of connectivity components from the graph
--
-- **NOTE:** Doesn't work properly. Check an example.
--
-- >>> components (((1 * 2) + (3 * 4)) + (2 * 3))
-- fromList [(4,[4,3,2,1])]
components :: Ord a => Graph a -> Map a [a]
components g = runST $ do
  (ps, g') <- mkVertexPoints g Map.empty
  componentsST g'
  componentsFromPoints $ Map.toList ps

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
  points <- extractPointsST g
  case points of
    (p:ps) ->
      foldM_ (\x y -> (UF.union x y) >> (return y)) p ps
    _ -> return ()
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

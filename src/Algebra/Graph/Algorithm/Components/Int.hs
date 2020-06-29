module Algebra.Graph.Algorithm.Components.Int where

import           Algebra.Graph
import           Algebra.Graph.Algorithm.Internal
import           Control.Monad
import           Control.Monad.ST
import           Data.UnionFind.ST                (Point)
import qualified Data.UnionFind.ST                as UF
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as Vector
import           Data.Vector.Mutable              (STVector)
import qualified Data.Vector.Mutable              as MVector

-- | O(s).
-- Extract connectivity components from the graph.
--
-- All vertices should have /int/ type.
--
-- First argument is amount of vertices,
-- second one is graph itself.
--
-- Examples:
--
-- >>> components 4 ((0 * 1) + (2 * 3))
-- [[1,0],[3,2]]
--
-- >>> components 4 (((0 * 1) + (2 * 3)) + (1 * 2))
-- [[3,2,1,0]]
components :: Int -> Graph Int -> [[Int]]
components n g = runST $ do
  ps <- MVector.replicate n Nothing
  (vs, g') <- mkVertexPoints g ps
  componentsST g'
  vs' <- Vector.unsafeFreeze vs
  comp <- componentsFromPoints n vs'
  comp' <- Vector.unsafeFreeze comp
  return $ filter (not . null) (Vector.toList comp')

-- | O(n).
componentsFromPoints :: Int -> Vector (Maybe (Point s Int)) -> ST s (STVector s [Int])
componentsFromPoints n ps = do
  vs <- MVector.replicate n []
  cfpHelper 0 n ps vs
  return vs

cfpHelper :: Int -> Int -> Vector (Maybe (Point s Int)) -> STVector s [Int] -> ST s ()
cfpHelper i n ps vs
  | i < n =
  case ps Vector.!? i of
    (Just (Just p)) -> do
      repr <- UF.descriptor p
      MVector.modify vs (i :) repr
      cfpHelper (i+1) n ps vs
    _ -> cfpHelper (i+1) n ps vs
  | otherwise = return ()

-- | O(s).
mkVertexPoints :: Graph Int -> STVector s (Maybe (Point s Int)) -> ST s (STVector s (Maybe (Point s Int)), Graph (Point s Int))
mkVertexPoints Empty ps = return (ps, Empty)
mkVertexPoints (Vertex v) ps = do
  el <- MVector.read ps v
  case el of
    Nothing -> do
      p <- UF.fresh v
      MVector.write ps v (Just p)
      return (ps, Vertex p)
    (Just p) -> return (ps, Vertex p)
mkVertexPoints (Connect l r) ps = do
  (lp, lg) <- mkVertexPoints l ps
  (rp, rg) <- mkVertexPoints r lp
  return (rp, Connect lg rg)
mkVertexPoints (Overlay l r) ps = do
  (lp, lg) <- mkVertexPoints l ps
  (rp, rg) <- mkVertexPoints r lp
  return (rp, Overlay lg rg)

-- | O(s).
componentsST :: Graph (Point s a) -> ST s ()
componentsST (Overlay l r) = componentsST l >> componentsST r
componentsST g@(Connect _ _) = do
  case extractVertices g of
    (p:ps) ->
      foldM_ (\x y -> (UF.union x y) >> (return y)) p ps
    _ -> return ()
componentsST _ = return ()

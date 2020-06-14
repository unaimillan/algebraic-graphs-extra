module BfsSpec where

import Test.Hspec
import Test.QuickCheck
import Algebra.Graph
import Algebra.Graph.Algorithm

sampleGraph :: Graph Int
sampleGraph = Overlay
  (Connect (Vertex 1) (Vertex 2))
  (Connect (Vertex 2) (Connect (Vertex 3) (Vertex 4)))

sampleGraph2 :: Graph Int
sampleGraph2 = Connect
  (Connect (Vertex 1) (Vertex 2))
  (Overlay (Vertex 3) (Vertex 4))

spec :: Spec
spec = do
  describe "Tests for  Breadth-first search" $ do
    it "BFS test 1" $ do
      (bfs sampleGraph 1) `shouldBe` [(2,1),(3,2),(4,2)]

    it "BFS test 2" $ do
      (bfs sampleGraph2 1) `shouldBe` [(2,1),(3,1),(4,1)]

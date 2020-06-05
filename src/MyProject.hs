module MyProject where

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

run :: IO ()
run = putStrLn "Hello, world!"

testBfs :: Int -> [(Int, Int)]
testBfs x = bfs sampleGraph x

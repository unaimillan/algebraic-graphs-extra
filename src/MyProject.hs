module MyProject where

import Algebra.Graph
import Algebra.Graph.Algorithm

sampleGraph :: Graph Int
sampleGraph = Overlay
  (Connect (Vertex 1) (Vertex 2))
  (Connect (Vertex 2) (Vertex 3))

run :: IO ()
run = putStrLn "Hello, world!"

test = shortestPath sampleGraph 1 3

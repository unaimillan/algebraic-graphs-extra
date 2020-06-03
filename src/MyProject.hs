module MyProject where

import Algebra.Graph
import Algebra.Graph.Algorithm

sampleGraph :: Graph Int
sampleGraph = Overlay
  (Connect (Vertex 1) (Vertex 2))
  (Connect (Vertex 2) (Connect (Vertex 3) (Vertex 4)))

run :: IO ()
run = putStrLn "Hello, world!"

test :: Maybe (Int, [Int])
test = shortestPath sampleGraph 1 4

testFindConnect :: Int -> [Graph Int]
testFindConnect x = findConnect sampleGraph x

testConnectedWith :: Int -> [Int]
testConnectedWith x = connectedWith sampleGraph x

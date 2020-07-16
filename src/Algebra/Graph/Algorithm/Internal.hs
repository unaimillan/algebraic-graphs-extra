module Algebra.Graph.Algorithm.Internal where

import           Algebra.Graph

-- | O(s).
extractVertices :: Graph a -> [a]
extractVertices Empty         = []
extractVertices (Vertex v)    = [v]
extractVertices (Connect l r) = extractVertices l <> extractVertices r
extractVertices (Overlay l r) = extractVertices l <> extractVertices r

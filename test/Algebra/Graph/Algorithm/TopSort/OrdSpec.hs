module Algebra.Graph.Algorithm.TopSort.OrdSpec where

import           Algebra.Graph
import           Algebra.Graph.Algorithm.TopSort.Ord
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "Special cases for TopSort.Ord module" $ do

    it "(1 * 2) + (2 * 3)" $ do
      let g = (1 * 2) + (2 * 3)
      let g_final = overlay (edge (0, 1) (1, 2)) (edge (1, 2) (2, 3))
      (topSort g) `shouldBe` (Just g_final)

    it "(1 * 2) + (2 * 3) + (3 * 1)" $ do
      let g = (1 * 2) + (2 * 3) + (3 * 1)
      (topSort g) `shouldBe` Nothing

    it "(2 * 1) * (2 + 3)" $ do
      let g = (2 * 1) * (2 + 3)
      (topSort g) `shouldBe` Nothing

    it "(1 * 2) + (3 * 2)" $ do
      let g = (1 * 2) + (3 * 2)
      let g_final = overlay (edge (0, 1) (1, 2)) (edge (2, 3) (1, 2))
      (topSort g) `shouldBe` (Just g_final)

  describe "General cases for TopSort.Ord module" $ do

    it "Path graph" $ property $ \n ->
      topSort (path [1..n]) == Just (path $ zip [0..] [1..(n :: Int)])

    it "Circuit graph" $ property $ \n ->
      topSort (circuit [1..(abs (n :: Int) + 1)]) == Nothing

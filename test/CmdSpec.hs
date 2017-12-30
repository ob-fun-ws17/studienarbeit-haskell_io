{-# LANGUAGE ScopedTypeVariables #-}
module LibSpec (spec) where

import Lib (square)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
describe "square" $ do
it "calculates the square of 5.3" $
square 5.3 `shouldBe` 28.09
it "calculates the square of an arbitrary integer" $
property $ \(n :: Integer) -> square n == n * n
it "calculates the square of an arbitrary double" $
property $ \(n :: Double) -> square n == n * n
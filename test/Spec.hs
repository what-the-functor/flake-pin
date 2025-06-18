module Main (main) where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "flake-pin" $ do
    it "is not yet implemented" $ do
      True `shouldBe` True

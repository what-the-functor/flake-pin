{-# LANGUAGE OverloadedStrings #-}

module FlakePin.FlakeInputNameSpec (spec) where

import Data.Text qualified as Text
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec.Core.Spec (Spec, describe, it)
import Test.Hspec.Hedgehog

import FlakePin.Types

spec :: Spec
spec = describe "FlakeInputName" $ do
    it "accepts valid input names" $ hedgehog acceptValidName

acceptValidName :: PropertyT IO ()
acceptValidName = do
    name <- forAll genValidName
    case mkFlakeInputName name of
        Right _ -> success
        Left err -> footnoteShow err >> failure

genValidName :: Gen Text.Text
genValidName = do
    firstChar <- Gen.choice [Gen.alpha, pure '_']
    remaining <- Gen.string (Range.linear 0 24) (Gen.choice [Gen.alphaNum, pure '_', pure '_'])
    pure $ Text.pack (firstChar : remaining)

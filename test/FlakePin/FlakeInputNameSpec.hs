{-# LANGUAGE OverloadedStrings #-}

module FlakePin.FlakeInputNameSpec (spec) where

import Data.Text (Text)
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
    it "rejects empty input names" $ hedgehog rejectEmpty
    it "rejects input names starting with a digit" $ hedgehog rejectFirstCharIsDigit

acceptValidName :: PropertyT IO ()
acceptValidName = do
    name <- forAll genValidName
    case mkFlakeInputName name of
        Right _ -> success
        Left err -> footnoteShow err >> failure

rejectEmpty :: PropertyT IO ()
rejectEmpty = do
    name <- forAll (pure Text.empty)
    case mkFlakeInputName name of
        Left EmptyInputName -> success
        Left _ -> failure
        Right _ -> failure

rejectFirstCharIsDigit :: PropertyT IO ()
rejectFirstCharIsDigit = do
    name <- forAll genStartsWithDigit
    case mkFlakeInputName name of
        Left FirstCharIsDigit -> success
        Left _ -> failure
        Right _ -> failure

genValidName :: Gen Text
genValidName = genFirstChar . Gen.choice $ [Gen.alpha, pure '_']

genStartsWithDigit :: Gen Text
genStartsWithDigit = genFirstChar Gen.digit

genFirstChar :: (MonadGen g) => g Char -> g Text
genFirstChar a = do
    firstChar <- a
    remaining <- Gen.string (Range.linear 0 24) (Gen.choice [Gen.alphaNum, pure '_', pure '-'])
    pure $ Text.pack (firstChar : remaining)

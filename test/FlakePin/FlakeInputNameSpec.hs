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
    it "rejects input names longer than 25 characters" $ hedgehog rejectLongerThanMaxLength

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

rejectLongerThanMaxLength :: PropertyT IO ()
rejectLongerThanMaxLength = do
    name <- forAll (genValidOfLength 26)
    case mkFlakeInputName name of
        Left (LongerThan 25) -> success
        Left _ -> failure
        Right _ -> failure

genValidName :: Gen Text
genValidName = genWithFirstChar . Gen.choice $ [Gen.alpha, pure '_']

genStartsWithDigit :: Gen Text
genStartsWithDigit = genWithFirstChar Gen.digit

genValidOfLength :: (MonadGen m) => Int -> m Text
genValidOfLength n = genName l l (Gen.choice [Gen.alpha, pure '_'])
  where
    l = n - 1

genWithFirstChar :: (MonadGen g) => g Char -> g Text
genWithFirstChar = genName 0 24

genName :: (MonadGen m) => Int -> Int -> m Char -> m Text
genName n1 n2 g = do
    firstChar <- g
    remaining <- Gen.string (Range.linear n1 n2) (Gen.choice [Gen.alphaNum, pure '_', pure '-'])
    pure $ Text.pack (firstChar : remaining)

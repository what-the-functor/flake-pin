{-# LANGUAGE DataKinds #-}
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
import Numeric.Natural (Natural)

spec :: Spec
spec = describe "FlakeInputName" $ do
    it "accepts valid input names" $ hedgehog acceptValidName
    it "rejects empty input names" $ hedgehog rejectEmpty
    it "rejects input names starting with a digit" $ hedgehog rejectFirstCharIsDigit
    it "rejects input names longer than 25 characters" $ hedgehog rejectLongerThanMaxLength
    it "rejects input names that are only whitespace" $ hedgehog rejectOnlyWhitespace
    it "rejects input names with internal whitespace" $ hedgehog rejectInternalWhitespace

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
    name <- forAll (genValidWithin (maxLength + 1) (maxLength + 1))
    case mkFlakeInputName name of
        Left (LongerThan 25) -> success
        Left _ -> failure
        Right _ -> failure

rejectOnlyWhitespace :: PropertyT IO ()
rejectOnlyWhitespace = do
    name <- forAll genOnlyWhitespace
    case mkFlakeInputName name of
        Left EmptyInputName -> success
        Left _ -> failure
        Right _ -> failure

rejectInternalWhitespace :: PropertyT IO ()
rejectInternalWhitespace = do
    name <- forAll genWithInternalWhitespace
    case mkFlakeInputName name of
        Left ContainsWhiteSpace -> success
        Left _ -> failure
        Right _ -> failure

genValidName :: Gen Text
genValidName = genWithFirstChar genValidHeadChar

genStartsWithDigit :: Gen Text
genStartsWithDigit = genWithFirstChar Gen.digit

genValidWithin :: Natural -> Natural -> Gen Text
genValidWithin minN maxN = genName minN maxN genValidHeadChar

genOnlyWhitespace :: Gen Text
genOnlyWhitespace = Gen.text (Range.linear 1 (fromIntegral maxLength)) genWhiteSpace

genWithInternalWhitespace :: Gen Text
genWithInternalWhitespace = do
    validName <- genValidWithin 2 (maxLength - 1)
    index <- Gen.int (Range.linear 1 (Text.length validName - 2))
    whiteSpace <- genWhiteSpace
    pure $
        let (a, b) = Text.splitAt index validName
         in Text.concat [a, Text.cons whiteSpace b]

genWithFirstChar :: Gen Char -> Gen Text
genWithFirstChar = genName 0 maxLength

genValidHeadChar :: Gen Char
genValidHeadChar = Gen.choice [Gen.alpha, pure '_']

genName :: Natural -> Natural -> Gen Char -> Gen Text
genName n1 n2 g1st = (charToText <$> g1st) <> genTailText n1 (n2 - 1)

genWhiteSpace :: Gen Char
genWhiteSpace = Gen.choice [pure ' ', pure '\t', pure '\n', pure '\r', pure '\xa0']

genTailText :: (MonadGen m) => Natural -> Natural -> m Text
genTailText minN maxN =
    Gen.text (Range.linear (fromIntegral minN) (fromIntegral maxN)) (Gen.choice [Gen.alphaNum, pure '_', pure '-'])

charToText :: Char -> Text
charToText = Text.pack . (: [])

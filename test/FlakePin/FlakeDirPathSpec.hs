{-# LANGUAGE OverloadedStrings #-}

module FlakePin.FlakeDirPathSpec (spec, genContents, genFileName) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.OsPath
import Test.Hspec.Core.Spec (Spec, describe, it)
import Test.Hspec.Hedgehog

import Data.List.NonEmpty qualified as NonEmpty
import FlakePin.Types
import System.OsPath qualified as OsPath

spec :: Spec
spec = do
    describe "FlakeFilePath" $ do
        it "accepts directories that contain a flake file" $ hedgehog acceptWithFlakeFile
        it "rejects directories that don't contain a flake file" $ hedgehog rejectMissingFlakeFile
    describe "isFlakeFile" $ do
        it "accepts a flake file" $ hedgehog acceptFlakeFile
        it "rejects any other file" $ hedgehog rejectFlakeFile

acceptWithFlakeFile :: PropertyT IO ()
acceptWithFlakeFile = do
    dirPath <- forAll genFileName
    flakeFile <- forAll genFlakeFileName
    xs <- forAll $ Gen.list (Range.linear 0 10) genFileName
    ys <- forAll $ Gen.list (Range.linear 0 10) genFileName
    let contents = NonEmpty.prependList xs (flakeFile :| ys)
    case mkFlakeDirPath dirPath contents of
        Right _ -> success
        Left _ -> failure

rejectMissingFlakeFile :: PropertyT IO ()
rejectMissingFlakeFile = do
    dirPath <- forAll genFileName
    contents <- forAll genContents
    case mkFlakeDirPath dirPath contents of
        Left NotFlakeDir -> success
        Right _ -> failure

acceptFlakeFile :: PropertyT IO ()
acceptFlakeFile = do
    file <- forAll genFlakeFileName
    (if isFlakeFile file then success else failure)

rejectFlakeFile :: PropertyT IO ()
rejectFlakeFile = do
    flakeFile <- forAll genFlakeFileName
    file <- forAll (Gen.filter (/= flakeFile) genFileName)
    (if isFlakeFile file then failure else success)

genFlakeFileName :: Gen OsPath
genFlakeFileName = Gen.mapMaybe id $ pure flakeFileName

genFileName :: Gen OsPath
genFileName =
    Gen.mapMaybeT OsPath.encodeUtf $ Gen.string (Range.linear 0 10) Gen.alphaNum

genNonEmpty :: Range Int -> Gen a -> Gen (NonEmpty a)
genNonEmpty range gen = do
    x <- gen
    xs <- Gen.list range gen
    pure (x :| xs)

genContents :: Gen (NonEmpty OsPath)
genContents = genNonEmpty (Range.linear 0 9) genFileName

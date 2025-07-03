{-# LANGUAGE OverloadedStrings #-}

module FlakePin.Path.Validation.InternalSpec (spec) where

import Control.Monad.Except (runExceptT)

import Data.List.NonEmpty qualified as NonEmpty
import System.IO.Error qualified as Error

import Hedgehog

import System.OsPath (OsPath)
import Test.Hspec.Core.Spec (Spec, describe, it)

import FlakePin.FlakeDirPathSpec (genContents, genFileName)
import FlakePin.Path.Validation.Internal

spec :: Spec
spec = do
    describe "checkExists" $ do
        it "accepts existing paths" acceptsExistingPaths
        it "rejects missing paths" rejectsMissingPaths
    describe "readContents" $ do
        it "reads the contents of directories" readsContentsOfDirectories
        it "reports filesystem read errors" reportsReadErrors
    describe "checkNonEmpty" $ do
        it "accepts directories with at least one entry" acceptsNonEmptyDirectories
        it "rejects empty directories" rejectsEmptyDirectories

acceptsExistingPaths :: PropertyT IO ()
acceptsExistingPaths = do
    path <- forAll genFileName
    let doesExist = const . Right $ True
        checkExists = mkCheckExists doesExist
    case runExceptT $ checkExists path of
        Right (Right _) -> success
        _ -> failure

rejectsMissingPaths :: PropertyT IO ()
rejectsMissingPaths = do
    path <- forAll genFileName
    let doesExist = const . Right $ False
        checkExists = mkCheckExists doesExist
    case runExceptT $ checkExists path of
        Right (Left DoesNotExist) -> success
        _ -> failure

readsContentsOfDirectories :: PropertyT IO ()
readsContentsOfDirectories = do
    path <- forAll genFileName
    contents <- forAll genContentsList
    let listDirectory = const . Right $ contents
        readContents = mkReadContents listDirectory
    case runExceptT $ readContents path of
        Right (Right (p, c)) | p == path && c == contents -> success
        _ -> failure

reportsReadErrors :: PropertyT IO ()
reportsReadErrors = do
    path <- forAll genFileName
    let listDirectory = const . Left $ Error.userError ""
        readContents = mkReadContents listDirectory
    case runExceptT $ readContents path of
        Right (Left (FailedToRead _)) -> success
        _ -> failure

acceptsNonEmptyDirectories :: PropertyT IO ()
acceptsNonEmptyDirectories = do
    path <- forAll genFileName
    contents <- forAll genContentsList
    case runExceptT $ checkNonEmpty (path, contents) of
        Right (Right (p, c)) | p == path && NonEmpty.toList c == contents -> success
        _ -> failure

rejectsEmptyDirectories :: PropertyT IO ()
rejectsEmptyDirectories = do
    path <- forAll genFileName
    case runExceptT $ checkNonEmpty (path, []) of
        Right (Left Empty) -> success
        _ -> failure

genContentsList :: Gen [OsPath]
genContentsList = NonEmpty.toList <$> genContents

module Main (main) where

import Control.Category ((>>>))
import Control.Exception (IOException, try)
import Control.Monad (unless)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError)
import Control.Monad.Extra ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty
import Data.List.NonEmpty qualified as NonEmpty
import System.Directory.OsPath qualified as Directory
import System.OsPath

-- | Errors that can occur when validating a directory and its contents
data PathError
    = DoesNotExist
    | FailedToRead IOException
    | Empty

validateDirectory :: OsPath -> IO (Either PathError (OsPath, NonEmpty OsPath))
validateDirectory = (checkExists >=> readContents >=> checkNonEmpty) >>> runExceptT
  where
    checkExists :: OsPath -> ExceptT PathError IO OsPath
    checkExists p = do
        exists <- liftIO $ Directory.doesDirectoryExist p
        unless exists $ throwError DoesNotExist
        pure p

    readContents :: OsPath -> ExceptT PathError IO (OsPath, [OsPath])
    readContents p = ExceptT $ do
        result <- try (Directory.listDirectory p)
        case result of
            Right contents -> pure $ Right (p, contents)
            Left (e :: IOException) -> pure $ Left $ FailedToRead e

    checkNonEmpty :: (OsPath, [OsPath]) -> ExceptT PathError IO (OsPath, NonEmpty OsPath)
    checkNonEmpty (p, paths) =
        liftEither $
            maybe
                (Left Empty)
                (Right . (p,))
                (NonEmpty.nonEmpty paths)

main :: IO ()
main = putStrLn "flake-pin not yet implemented"

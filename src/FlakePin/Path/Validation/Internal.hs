{- | Internal implementation details for path validation.

__Warning:__ This module is considered internal.
The API may change in any version without notice.
Use "FlakePin.Path.Validation" for the stable public API.
-}
module FlakePin.Path.Validation.Internal (
    PathError (..),
    mkCheckExists,
    mkReadContents,
    checkNonEmpty,
    mkValidateDirectory,
)
where

import Control.Exception (IOException)
import Control.Monad (unless)
import Control.Monad.Error.Class (
    MonadError (throwError),
    tryError,
 )
import Control.Monad.Except (ExceptT (..), runExceptT, withExceptT)
import Control.Monad.Extra ((>=>))

import Control.Category ((>>>))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import System.OsPath (OsPath)

type ErrorM m = MonadError IOException m
type DoesExist m = OsPath -> m Bool
type ListDirectory m = OsPath -> m [OsPath]
type Validation m a = ExceptT PathError m a
type Result m = m (Either PathError (OsPath, NonEmpty OsPath))

-- | Errors that can occur when validating a directory and its contents.
data PathError
    = DoesNotExist
    | FailedToRead IOException
    | Empty
    deriving (Show, Eq)

-- | Create a check for directory existence.
mkCheckExists ::
    (ErrorM m) =>
    DoesExist m ->
    OsPath ->
    Validation m OsPath
mkCheckExists doesDirectoryExist p = do
    exists <- withExceptT FailedToRead $ ExceptT $ tryError $ doesDirectoryExist p
    unless exists $ throwError DoesNotExist
    pure p

-- | Create a function to read directory contents.
mkReadContents :: (ErrorM m) => ListDirectory m -> OsPath -> Validation m (OsPath, [OsPath])
mkReadContents listDir p = do
    contents <- withExceptT FailedToRead $ ExceptT $ tryError $ listDir p
    pure (p, contents)

-- | Ensure a directory contains at least one entry.
checkNonEmpty :: (Monad m) => (OsPath, [OsPath]) -> Validation m (OsPath, NonEmpty OsPath)
checkNonEmpty (p, paths) =
    maybe (throwError Empty) (pure . (p,)) (NonEmpty.nonEmpty paths)

{- | Create a directory validator that checks if a directory exists and contains at least one entry.

The validator will return:

* @Left DoesNotExist@ if the directory doesn't exist
* @Left (FailedToRead e)@ if an IO exception occurs while checking existence or reading contents
* @Left Empty@ if the directory exists but is empty
* @Right (path, contents)@ if the directory exists and is non-empty

Example:

@
validateDirectory :: OsPath -> IO (Either PathError (OsPath, NonEmpty OsPath))
validateDirectory = mkValidateDirectory doesDirectoryExist listDirectory
@
-}
mkValidateDirectory :: (ErrorM m) => DoesExist m -> ListDirectory m -> OsPath -> Result m
mkValidateDirectory doesDirectoryExist listDirectory =
    (checkExists >=> readContents >=> checkNonEmpty) >>> runExceptT
  where
    checkExists = mkCheckExists doesDirectoryExist
    readContents = mkReadContents listDirectory

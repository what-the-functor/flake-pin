module FlakePin.Types (
    FlakeInputName (..),
    FlakeInputNameError (..),
    FlakeDirPath (..),
    FlakeDirPathError (..),
    maxLength,
    mkFlakeInputName,
    isFlakeFile,
    flakeFileName,
    mkFlakeDirPath,
) where

import Data.Char qualified as Char
import Data.Either ()
import Data.List.NonEmpty
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural (Natural)
import System.OsPath
import System.OsPath qualified as OsPath

maxLength :: Natural
maxLength = 25

flakeFileName :: Maybe OsPath
flakeFileName = OsPath.encodeUtf "flake.nix"

--- Types ---

{- | A valid flake input name:
- contains at least one character,
- does not start with a digit,
- is not longer than the 'maxLength'.
- does not contain internal whitespace (whitespace at the ends is trimmed),
-}
newtype FlakeInputName = FlakeInputName Text

-- | A valid flake directory (contains a flake file at the root of the directory).
newtype FlakeDirPath = FlakeDirPath OsPath

data FlakeInputNameError
    = EmptyInputName
    | FirstCharIsDigit
    | LongerThan Natural
    | ContainsWhiteSpace
    deriving (Show, Eq)

data FlakeDirPathError = NotFlakeDir

--- Smart Constructors ---

mkFlakeInputName :: Text -> Either FlakeInputNameError FlakeInputName
mkFlakeInputName text = case Text.uncons . Text.strip $ text of
    Nothing -> Left EmptyInputName
    Just (first, _) | Char.isDigit first -> Left FirstCharIsDigit
    Just (_, cs) | Text.length cs >= fromIntegral maxLength -> Left (LongerThan maxLength)
    Just (_, cs) | Text.any Char.isSpace cs -> Left ContainsWhiteSpace
    Just _ -> Right (FlakeInputName text)

mkFlakeDirPath :: OsPath -> NonEmpty OsPath -> Either FlakeDirPathError FlakeDirPath
mkFlakeDirPath path contents =
    case NonEmpty.filter isFlakeFile contents of
        [] -> Left NotFlakeDir
        _ -> Right $ FlakeDirPath path

--- Predicates ---

{- | Check if a filename is a valid flake file.
This validates filenames relative to a flake directory.
Only "flake.nix" is considered a valid flake filename.
-}
isFlakeFile :: OsPath -> Bool
isFlakeFile a = all (a ==) flakeFileName

module FlakePin.Types (
    FlakeInputName (..),
    FlakeInputNameError (..),
    mkFlakeInputName,
) where

import Data.Char qualified as Char
import Data.Either ()
import Data.Text (Text)
import Data.Text qualified as Text

newtype FlakeInputName = FlakeInputName Text

mkFlakeInputName :: Text -> Either FlakeInputNameError FlakeInputName
mkFlakeInputName text = case Text.uncons text of
    Nothing -> Left EmptyInputName
    Just (first, _) | Char.isDigit first -> Left FirstCharIsDigit
    Just _ -> Right (FlakeInputName text)

data FlakeInputNameError
    = EmptyInputName
    | FirstCharIsDigit
    deriving (Show, Eq)

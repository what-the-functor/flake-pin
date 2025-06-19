module FlakePin.Types (
    FlakeInputName (..),
    FlakeInputNameError (..),
    mkFlakeInputName,
) where

import Data.Either ()
import Data.Text (Text)
import Data.Text qualified as Text

newtype FlakeInputName = FlakeInputName Text

mkFlakeInputName :: Text -> Either FlakeInputNameError FlakeInputName
mkFlakeInputName text
    | Text.null text = Left EmptyInputName
    | otherwise = Right (FlakeInputName text)

data FlakeInputNameError
    = EmptyInputName
    deriving (Show, Eq)

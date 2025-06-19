module FlakePin.Types (
    FlakeInputName (..),
    FlakeInputNameError (..),
    mkFlakeInputName,
) where

import Data.Either ()
import Data.Text (Text)

newtype FlakeInputName = FlakeInputName Text

mkFlakeInputName :: Text -> Either FlakeInputNameError FlakeInputName
mkFlakeInputName text = Right (FlakeInputName text)

data FlakeInputNameError
    = EmptyInputName
    deriving (Show, Eq)

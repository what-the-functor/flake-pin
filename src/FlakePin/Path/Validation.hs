{- | Public API for path validation.

For internal functions, see "FlakePin.Path.Validation.Internal".
-}
module FlakePin.Path.Validation (
    PathError (..),
    mkValidateDirectory,
) where

import FlakePin.Path.Validation.Internal (
    PathError (..),
    mkValidateDirectory,
 )

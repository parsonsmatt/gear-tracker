-- | The project prelude for GT. Import this instead of the "Prelude".
module GT.Prelude
    ( module Prelude
    , Text
    , MonadIO(..)
    ) where

import           Prelude

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Text              (Text)

-- | The project prelude for GT. Import this instead of the "Prelude".
module GT.Prelude
    ( todo
    , module Prelude
    , Text
    , module Control.Monad
    , module Control.Monad.IO.Class
    , module Data.Time
    , module Data.Ratio
    , module Data.Traversable
    , module Data.Foldable
    ) where

import Prelude

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Ratio
import Data.Text
    ( Text
    )
import Data.Time
import Data.Traversable

-- | A placeholder stub.
todo :: String -> a
todo message = error ("TODO: " <> message)

{-# WARNING todo "todo remains in code!" #-}

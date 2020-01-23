module Spec.GT.DB where

import Spec.GT.DB.Prelude

import qualified Spec.GT.DB.Component as Component
import qualified Spec.GT.DB.Usage     as Usage

spec :: Spec
spec = provideDatabase $ do
    describe "Component" Component.spec
    describe "Usage" $ Usage.spec

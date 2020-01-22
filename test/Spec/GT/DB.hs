module Spec.GT.DB where

import Spec.GT.DB.Prelude

import qualified Spec.GT.DB.Usage       as Usage

spec :: Spec
spec = provideDatabase $ do
    describe "Usage" Usage.spec

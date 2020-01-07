module Spec.GT.DB where

import Test.Hspec

import qualified Spec.GT.DB.Usage as Usage

spec :: Spec
spec = do
    describe "Usage" Usage.spec

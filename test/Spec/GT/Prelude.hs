module Spec.GT.Prelude
    ( module GT.Prelude
    , module Test.Hspec.Expectations.Lifted
    , module Test.Hspec
    ) where

import GT.Prelude

import Test.Hspec                     hiding
    ( shouldBe
    , shouldContain
    , shouldEndWith
    , shouldMatchList
    , shouldNotBe
    , shouldNotContain
    , shouldNotReturn
    , shouldNotSatisfy
    , shouldReturn
    , shouldSatisfy
    , shouldStartWith
    , expectationFailure
    )
import Test.Hspec.Expectations.Lifted

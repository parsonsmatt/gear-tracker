module Main where

import Spec.GT.Prelude

import qualified Spec.GT.DB

main :: IO ()
main =
    hspec $ do
        describe "Spec.GT.DB" $ do
            Spec.GT.DB.spec

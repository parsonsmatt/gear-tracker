module Spec.GT.DB.Usage where

import Spec.GT.DB.Prelude

import           Control.Monad.Logger
import           Data.Time.Clock
import           Data.Traversable
import           Database.Persist.Postgresql
import qualified Database.Postgres.Temp      as Temp

import GT.DB.Schema.Bike
import GT.DB.Schema.Component
import GT.DB.Schema.Migration
import GT.DB.Schema.Ride
import GT.DB.Usage            as Usage

spec :: SpecWith TestDb
spec = do
    describe "new" $ do
        it "works" $ \conn -> do
            runTestDb conn $ do
                frameId <- insert Component
                    { componentName = "Scrubtrout"
                    , componentPart = "Frame"
                    , componentBrand = "Salsa"
                    , componentModel = "Cutthroat"
                    , componentDescription = ""
                    , componentParent = Nothing
                    }
                bikeId <- insert $ Bike frameId
                wheelId <- insert Component
                    { componentName = "Good wheel"
                    , componentPart = "Front wheel"
                    , componentBrand = "WTB"
                    , componentModel = "Asym 29"
                    , componentDescription = "it is p good"
                    , componentParent = Just frameId
                    }
                let tire = Component
                        { componentName = "knobbss"
                        , componentPart = "Tire"
                        , componentBrand = "WTB"
                        , componentModel = "Riddler 45"
                        , componentDescription = ""
                        , componentParent = Just wheelId
                        }
                tireId <- insert tire
                rideId <- insert Ride
                    { rideDistance = 32
                    , rideDate = UTCTime (fromGregorian 2020 01 03) 0
                    }

                usages <- Usage.new bikeId rideId
                map entityVal usages
                    `shouldBe`
                        [ Usage rideId frameId
                        , Usage rideId wheelId
                        , Usage rideId tireId
                        ]

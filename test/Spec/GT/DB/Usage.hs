module Spec.GT.DB.Usage where

import Spec.GT.Prelude

import           Control.Monad.Logger
import           Data.Time.Clock
import           Database.Persist.Postgresql
import qualified Database.Postgres.Temp      as Temp

import GT.DB.Schema.Bike
import GT.DB.Schema.Component
import GT.DB.Schema.Ride
import GT.DB.Usage            as Usage

spec :: Spec
spec = do
    describe "new" $ do
        it "works" $ do
            pendingWith "really need a better database testing story!!"
            usageIds <-
                Temp.with $ \db -> runNoLoggingT $
                withPostgresqlConn (Temp.toConnectionString db) $ \conn -> do
                    flip runSqlConn conn $ do
                        componentId <- insert Component
                            { componentName = "Scrubtrout"
                            , componentPart = "Frame"
                            , componentBrand = "Salsa"
                            , componentModel = "Cutthroat"
                            , componentDescription = ""
                            , componentParent = Nothing
                            }
                        bikeId <- insert $ Bike componentId
                        wheelId <- insert Component
                            { componentName = "Good wheel"
                            , componentPart = "Front wheel"
                            , componentBrand = "WTB"
                            , componentModel = "Asym 29"
                            , componentDescription = "it is p good"
                            , componentParent = Just componentId
                            }
                        rideId <- insert Ride
                            { rideDistance = 32
                            , rideDate = UTCTime (fromGregorian 2020 01 03) 0
                            }

                        Usage.new bikeId rideId
            length usageIds `shouldBe` 2

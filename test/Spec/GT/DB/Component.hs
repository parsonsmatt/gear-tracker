module Spec.GT.DB.Component where

import Spec.GT.DB.Prelude

import Database.Persist.Postgresql

import GT.DB.Schema.Component
import GT.DB.Schema.Bike
import qualified GT.DB.Component as Component

spec :: SpecWith TestDb
spec = do
    describe "listForBike" $ do
        it "works" $ \conn -> do
            runTestDb conn $ do
                let frame = Component
                        { componentName = "Scrubtrout"
                        , componentPart = "Frame"
                        , componentBrand = "Salsa"
                        , componentModel = "Cutthroat"
                        , componentDescription = ""
                        , componentParent = Nothing
                        }
                frameId <- insert frame
                bikeId <- insert $ Bike frameId
                let wheel = Component
                        { componentName = "Good wheel"
                        , componentPart = "Front wheel"
                        , componentBrand = "WTB"
                        , componentModel = "Asym 29"
                        , componentDescription = "it is p good"
                        , componentParent = Just frameId
                        }
                let handlebars = Component
                        { componentName = "bars"
                        , componentPart = "handlebars"
                        , componentBrand = "Salsa"
                        , componentModel = "Cowchipper 46"
                        , componentDescription = ""
                        , componentParent = Just frameId
                        }
                wheelId <- insert wheel
                let tire = Component
                        { componentName = "knobbss"
                        , componentPart = "Tire"
                        , componentBrand = "WTB"
                        , componentModel = "Riddler 45"
                        , componentDescription = ""
                        , componentParent = Just wheelId
                        }
                tireId <- insert tire
                handlebarId <- insert handlebars
                let frame2 = Component
                        { componentName = "Chonker"
                        , componentPart = "Frame"
                        , componentBrand = "Salsa"
                        , componentModel = "Mukluk"
                        , componentDescription = ""
                        , componentParent = Nothing
                        }
                frame2Id <- insert frame2
                bike2Id <- insert $ Bike frame2Id
                flatboys <- insert Component
                    { componentName = ""
                    , componentPart = "Handlebars"
                    , componentBrand = "Salsa"
                    , componentModel = "Rustler"
                    , componentDescription = ""
                    , componentParent = Just frame2Id
                    }

                components <- Component.componentsForBike bikeId
                map (componentName . entityVal) components
                    `shouldMatchList`
                        map componentName [ wheel, tire, handlebars, frame ]

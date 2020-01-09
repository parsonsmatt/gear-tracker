{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

-- | Import data from Strava.
--
-- This module uses the @Strive@ library.
module GT.Strava where

import GT.Prelude

import           Control.Monad.Logger
import           Data.Maybe
import           Data.String
import qualified Data.Text            as Text
import           Database.Persist.Sql as DB
import           Strive               hiding
    ( map
    )

import qualified GT.DB.Schema.StravaActivity as DB
import qualified GT.DB.Schema.StravaGear     as DB

-- | Scaffolding. Ignore this.
type Runner = forall x m. SqlPersistT m x -> m x

-- | A draft implementatino of gear and activity import. This
-- implementation does not currently handle Strava's rate limiting.
runImport
    :: (MonadLogger m, MonadIO m)
    => Runner
    -> Text -- ^ Strava authorization token
    -> m ()
runImport runDB token = do
    logInfoN "Beginning import. . ."

    client <- liftIO $ buildClient (Just token)
    logDebugN "Created client."

    loop client 1
  where
    loop client pageNumber = do
        eresult <- liftIO $
            getCurrentActivities client (with [set page pageNumber, set perPage 200])

        case eresult of
            Left (_resp, str) -> do
                logErrorN $
                    "Received Left from getCurrentActivities: " <> fromString str
            Right [] ->
                logInfoN "We're done!"
            Right results -> do
                logInfoN "Importing Gear..."
                for_ (mapMaybe (Strive.get gearId) results) $ \gearId -> do
                    logInfoN $ "Checking presence of gear " <> gearId <> " in the database."
                    mgear <- runDB $ DB.getBy (DB.UniqueGearStravaId gearId)
                    case mgear of
                        Nothing -> do
                            estravaGear <- liftIO $ getGear client (Text.unpack gearId)
                            case estravaGear of
                                Left (_resp, str') ->
                                    logErrorN $ "Received Left from getCurrentActivities: " <> fromString str'
                                Right stravaGear -> do
                                    logInfoN "Gear retrieved from Strava. Inserting..."
                                    void $ runDB $ DB.upsert (mkGearFromStrava stravaGear) []
                                    todo "Associate the Strava gear with the user's gear"
                        Just _gear ->
                            logInfoN "Gear already present in the database."

                runDB $ DB.putMany (map mkActivityFromStrava results)

                loop client (pageNumber + 1)

mkGearFromStrava :: GearDetailed -> DB.StravaGear
mkGearFromStrava GearDetailed {..} =
    DB.StravaGear
        { DB.stravaGearStravaId =
            gearDetailed_id
        , DB.stravaGearBrandName =
            gearDetailed_brandName
        , DB.stravaGearModelName =
            gearDetailed_modelName
        , DB.stravaGearDescription =
            gearDetailed_description
        , DB.stravaGearDistance =
            gearDetailed_distance
        }

mkActivityFromStrava :: ActivitySummary -> DB.StravaActivity
mkActivityFromStrava ActivitySummary {..} =
    DB.StravaActivity
        { DB.stravaActivityStravaId =
            fromIntegral activitySummary_id
        , DB.stravaActivityMetaAthlete =
            fromIntegral (athleteMeta_id activitySummary_athlete)
        , DB.stravaActivityDistance =
            activitySummary_distance
        }

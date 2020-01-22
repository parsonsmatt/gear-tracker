-- | A 'Usage' links a 'Component' with a 'Ride'. This link is what
-- provides the information about distance on a per-component basis.
module GT.DB.Usage
    ( module GT.DB.Usage
    , module GT.DB.Schema.Usage
    ) where

import GT.DB.Prelude

import Database.Persist.Sql        (get, insertEntity)
import Database.Persist.Sql.Raw.QQ

import qualified GT.DB.Component        as Component
import           GT.DB.Schema.Bike
import           GT.DB.Schema.Component
import           GT.DB.Schema.Ride
import           GT.DB.Schema.Usage

-- | Create a new 'Usage' for all components on the 'Bike' for the given
-- 'Ride'.
--
-- TODO: esqueleto does not support CTEs. It should!
new :: (MonadIO m) => BikeId -> RideId -> SqlPersistT m [Entity Usage]
new bikeId rideId = do
    mride <- get rideId
    case mride of
        Nothing ->
            pure []
        Just _ -> do
            components <- Component.componentsForBike bikeId
            for components $ \(Entity componentId _) ->
                insertEntity Usage
                    { usageComponent = componentId
                    , usageRide = rideId
                    }

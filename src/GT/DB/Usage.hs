-- | A 'Usage' links a 'Component' with a 'Ride'. This link is what
-- provides the information about distance on a per-component basis.
module GT.DB.Usage
    ( module GT.DB.Usage
    , module GT.DB.Schema.Usage
    ) where

import GT.DB.Prelude

import Database.Persist.Sql.Raw.QQ
import Database.Persist.Sql (insert, get)

import GT.DB.Schema.Bike
import GT.DB.Schema.Component
import GT.DB.Schema.Usage
import GT.DB.Schema.Ride

-- | Create a new 'Usage' for all components on the 'Bike' for the given
-- 'Ride'.
--
-- TODO: esqueleto does not support CTEs. It should!
new :: (MonadIO m) => BikeId -> RideId -> SqlPersistT m [UsageId]
new bikeId rideId = do
    mride <- get rideId
    case mride of
        Nothing ->
            pure []
        Just _ -> do
            components <- [sqlQQ|
WITH RECURSIVE children AS (
    SELECT c.*
    FROM component AS c

    UNION ALL

    SELECT c_r.*
    FROM component AS c_r
    INNER JOIN children AS c
        ON c_r.parent = c.id
    )
SELECT ??
FROM bike AS b
INNER JOIN children AS component
    ON component.parent = b.component
WHERE b.component = #{bikeId}
                |]

            for components $ \(Entity componentId _) ->
                insert Usage
                    { usageComponent = componentId
                    , usageRide = rideId
                    }

-- | A 'Usage' links a 'Component' with a 'Ride'. This link is what
-- provides the information about distance on a per-component basis.
module GT.DB.Usage
    ( module GT.DB.Usage
    , module GT.DB.Schema.Usage
    ) where

import GT.DB.Prelude

import Database.Persist.Sql.Raw.QQ
import Database.Persist.Sql (insert)

import GT.DB.Schema.Bike
import GT.DB.Schema.Component
import GT.DB.Schema.Usage
import GT.DB.Schema.Ride

-- | Create a new 'Usage' for all components on the 'Bike' for the given
-- 'Ride'.
new :: (MonadIO m) => BikeId -> RideId -> SqlPersistT m [UsageId]
new bikeId rideId = do
    components <- [sqlQQ|
WITH RECURSIVE children AS (
    SELECT *
    FROM component
    UNION ALL
    SELECT c_r.*
    FROM component AS c_r
    INNER JOIN children AS c
        ON c_r.parent_id = c.id
    )
SELECT ??
FROM bike AS b
INNER JOIN children
    ON children.parent_id = b.id
WHERE b.id = #{bikeId}
        |]
    for components $ \(Entity componentId _) -> do
        insert Usage
            { usageComponent = componentId
            , usageRide = rideId
            }

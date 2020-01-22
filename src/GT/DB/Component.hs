-- | This module exports types and queries for working with 'Component's.
module GT.DB.Component
    ( module GT.DB.Component
    , module GT.DB.Schema.Component
    ) where

import GT.DB.Prelude

import Database.Esqueleto
import Database.Persist.Sql.Raw.QQ

import GT.DB.Schema.Component
import GT.DB.Schema.Ride
import GT.DB.Schema.Bike
import GT.DB.Schema.Usage

-- | Return the total mileage for the given 'ComponentId'.
mileage
    :: ComponentId
    -> SqlQuery (SqlExpr (Value (Maybe Rational)))
mileage componentId =
    from $ \(usage `InnerJoin` ride) -> do
    on $ usage ^. UsageRide ==. ride ^. RideId
    where_ $ usage ^. UsageComponent ==. val componentId
    pure $ sum_ $ ride ^. RideDistance

componentsForBike :: MonadIO m => BikeId -> SqlPersistT m [Entity Component]
componentsForBike bikeId =
    [sqlQQ|
WITH RECURSIVE children AS (
    SELECT c.*
    FROM component AS c

    UNION

    SELECT c_r.*
    FROM component AS c_r
    INNER JOIN children AS c
        ON c_r.id = c.parent
    )
SELECT ??
FROM component AS c
INNER JOIN children AS component
    ON c.id = component.parent
    OR c.id = component.id
    OR c.parent = component.id
WHERE c.id = #{bikeId}
    |]

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

getForBike :: MonadIO m => BikeId -> SqlPersistT m [Entity Component]
getForBike bikeId =
    [sqlQQ|
WITH RECURSIVE children AS (
    SELECT c.*
    FROM component AS c
    WHERE c.id = #{bikeId}

    UNION

    SELECT c_r.*
    FROM component AS c_r
    INNER JOIN children AS c
        ON c.id = c_r.parent
    )
SELECT ??
FROM children AS component
    |]

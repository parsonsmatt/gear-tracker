-- | A 'StravaGear' is the database table we use to store the bikes
-- imported from Strava.
module GT.DB.Schema.StravaGear where

import GT.DB.Prelude

mkModel $(loadModel "StravaGear")

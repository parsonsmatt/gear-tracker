-- | A 'Usage' is an instance of a 'Component' being used on a 'Ride'.
module GT.DB.Schema.Usage where

import GT.DB.Prelude

import GT.DB.Schema.Ride
import GT.DB.Schema.Component

mkModel $(loadModel "Usage")

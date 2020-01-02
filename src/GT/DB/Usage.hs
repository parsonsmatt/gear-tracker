module GT.DB.Usage where

import GT.DB.Prelude

import GT.DB.Ride
import GT.DB.Component

mkModel $(loadModel "Usage")

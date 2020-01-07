module GT.DB.Schema.Bike where

import GT.DB.Prelude

import GT.DB.Schema.Component

mkModel $(loadModel "Bike")

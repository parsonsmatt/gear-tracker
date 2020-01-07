-- | A 'Bike' is a special case of a 'Component' - you can think of it as
-- a top-level equipment configuration that specifies a number of other
-- component options as well.
module GT.DB.Schema.Bike where

import GT.DB.Prelude

import GT.DB.Schema.Component

mkModel $(loadModel "Bike")

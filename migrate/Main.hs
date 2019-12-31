-- | Run the database migrations.
module Main where

import GT.Prelude

import Data.String (IsString(..))
import Database.Persist.Postgresql
import Control.Monad.Logger

import GT.DB.Migration

connstr :: String
connstr = "dbname=geartracker"

main :: IO ()
main =
    runStdoutLoggingT
    $ withPostgresqlConn (fromString connstr)
    $ runSqlConn (runMigration migrateAll)


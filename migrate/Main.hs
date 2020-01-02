-- | Run the database migrations. Currently doesn't take any arguments and
-- assumes the database to be available to the current user with the
-- database name @geartracker@.
module Main where

import GT.Prelude

import Control.Monad.Logger
import Data.String
import Database.Persist.Postgresql

import GT.DB.Migration

connstr :: String
connstr = "dbname=geartracker"

main :: IO ()
main =
    runStdoutLoggingT
    $ withPostgresqlConn (fromString connstr)
    $ runSqlConn (runMigration migrateAll)

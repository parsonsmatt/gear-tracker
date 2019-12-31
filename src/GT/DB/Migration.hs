-- | This module exists to provide @persistent@ automigrations. Eventually,
-- that feature will be removed in favor of manual migrations.
module GT.DB.Migration where

import GT.DB.Prelude

import Data.List
import System.Directory

import Database.Persist.Quasi
import Database.Persist.Sql
import Database.Persist.TH

mkMigrate "migrateAll" $(do
    files <-
        map ("schema/" <>)
            . filter (".persistentmodels" `isSuffixOf`)
            <$> liftIO (getDirectoryContents "schema")
    persistManyFileWith lowerCaseSettings files
    )

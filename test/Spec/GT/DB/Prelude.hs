{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.GT.DB.Prelude
    ( module Spec.GT.Prelude
    , aroundAll
    , TestDb
    , runTestDb
    , provideDatabase
    ) where

import Spec.GT.Prelude

import UnliftIO

import           Control.Monad.Logger
import           Database.Persist.Postgresql
import qualified Database.Postgres.Temp      as Temp
import           Test.Hspec

import           GT.DB.Schema.Migration

-- https://github.com/hspec/hspec/issues/255#issuecomment-568933195
aroundAll :: forall a. ((a -> IO ()) -> IO ()) -> SpecWith a -> Spec
aroundAll withFunc specWith = do
  (var, stopper, asyncer) <- runIO $
    (,,) <$> newEmptyMVar <*> newEmptyMVar <*> newIORef Nothing
  let theStart :: IO a
      theStart = do

        thread <- async $ do
          withFunc $ \x -> do
            putMVar var x
            takeMVar stopper
          pure $ error "Don't evaluate this"

        writeIORef asyncer $ Just thread

        either pure pure =<< (wait thread `race` takeMVar var)

      theStop :: a -> IO ()
      theStop _ = do
        putMVar stopper ()
        traverse_ cancel =<< readIORef asyncer

  beforeAll theStart $ afterAll theStop $ specWith

newtype TestDb = TestDb { unTestDb :: SqlBackend }

runTestDb :: TestDb -> SqlPersistT IO a -> IO a
runTestDb (TestDb conn) action =
    flip runSqlConn conn $ do
        result <- action
        transactionUndo
        pure result


provideDatabase :: SpecWith TestDb -> Spec
provideDatabase = do
    aroundAll $ \action -> do
        eresult <- Temp.with $ \db ->
            runNoLoggingT $
            withPostgresqlConn (Temp.toConnectionString db) $ \conn -> do
                runSqlConn (runMigrationSilent migrateAll) conn
                liftIO $ action $ TestDb conn
        case eresult of
            Left e -> throwM e
            Right _ -> pure ()


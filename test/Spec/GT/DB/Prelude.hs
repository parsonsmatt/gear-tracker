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

-- | A 'TestDb' is a newtype wrapper around the @persistent@ 'SqlBackend'
-- type. The default way to use this is to call 'runTestDb', which performs
-- the action, returns the result, and then undoes the transaction, which
-- should allow for fast database tests.
newtype TestDb = TestDb { unTestDb :: SqlBackend }

-- | Run the given action and rollback the transaction that created it.
-- This allows you to insert whatever records you need into the database
-- while not having to worry about cleaning them up.
runTestDb :: TestDb -> SqlPersistT IO a -> IO a
runTestDb (TestDb conn) action =
    flip runSqlConn conn $ do
        result <- action
        transactionUndo
        pure result

-- | Provide a shared database connection to the tests. This summons up
-- a database connection, runs migrations, and passes it to the test
-- suites scoped under it.
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

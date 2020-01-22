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

import qualified Debug.Trace as Debug
import           UnliftIO

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.Text                   as Text
import           Database.Persist.Postgresql
import qualified Database.Postgres.Temp      as Temp
import           Test.Hspec
import           Text.Shakespeare.Text       (st)

import GT.DB.Schema.Migration

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
                flip runSqlConn conn $ do
                    runMigrationSilent (const id makeUnlogged migrateAll)
                liftIO $ action $ TestDb conn
        case eresult of
            Left e -> throwM e
            Right _ -> pure ()

makeUnlogged :: Migration -> Migration
makeUnlogged =
    mapWriterT (censor (fmap (fmap (Text.replace "CREATe TABLE" "CREATe UNLOGGED TABLE"))))

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
--
-- lifted from yesod scaffold
wipeDB :: MonadIO m => SqlPersistT m ()
wipeDB = do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = map (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " <> Text.intercalate ", " escapedTables
    rawExecute query []

-- lifted from yesod scaffold
getTables :: MonadIO m => SqlPersistT m [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public'
        AND table_type = 'BASE TABLE';
    |] []

    return $ map unSingle tables

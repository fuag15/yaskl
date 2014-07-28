{-# LANGUAGE OverloadedStrings #-}

-- | The guts, actually run through and do the right migrations on our database based on 'Yaskl.Data.Arguments' and 'Yaskl.Data.Config'
module Yaskl.Migrator (dispatchTasks) where

import Yaskl.Data.Migration    as M ( Migration(..)
                                    , Direction(..)
                                    , MigrationVersion )

import Yaskl.Data.Arguments    as A ( Arguments(..)
                                    , Action(..)
                                    , Version(..)   )

import Yaskl.Util.Migration         ( latestMigrationFilter
                                    , boundedMigrateUpFilter
                                    , boundedMigrateDownFilter )

import Yaskl.Data.Config       as C (Environment(..), Config(..))
import Yaskl.Data.Database          (TableName, DatabaseHandler(..))
import Control.Monad                (mapM, liftM)
import Data.Int                     (Int64)
import Data.Word                    (Word16)

-- | Takes a program state and runs the appropriate migrations on it
dispatchTasks :: DatabaseHandler a
              => Arguments         -- ^ parsed arguments
              -> C.Config a        -- ^ parsed Configurations
              -> IO ()
dispatchTasks args config = do
  let requestedenv   = environment args
      targetenv      = head $ filter ((== requestedenv) . name) $ environments config
      targetdatabase = head $ databases targetenv
      migrationtable = C.migrationTable config
      targetversion  = A.version args
  database <- initializeConnection targetdatabase
  case action args of
    Create  -> createDatabase migrationtable targetversion (bases config) database
    Migrate -> liftM sum $ runMigrations targetversion (migrations config) migrationtable database
    Seed    -> runMigration migrationtable Up database $ seed config
  closeConnection database

-- | Creates the migration table in the database and then runs the base migration related to the desired major version
createDatabase :: DatabaseHandler a
               => TableName         -- ^ Current migration table (storage for migration information)
               -> Version           -- ^ initial version
               -> [Migration]       -- ^ list of available migrations
               -> DBConnection a    -- ^ Active database connection
               -> IO Int            -- ^ Number of affected rows
createDatabase migrationtable targetversion basemigrations database = do
  initializeMigrations migrationtable database
  case targetversion of
    Target ver -> runMigration migrationtable Up database $ head $ filter ((== ver) . M.version) basemigrations
    Latest     -> runMigration migrationtable Up database $ last basemigrations

-- | Run through all relevant migrations
runMigrations :: DatabaseHandler a
              => Version           -- ^ Target Version
              -> [Migration]       -- ^ Available migrations
              -> TableName         -- ^ Table that holds migration data
              -> DBConnection a    -- ^ Active db connection
              -> IO [Int]          -- ^ Number of tables affected per migration
runMigrations targetversion migrations migrationtable database = do
  currentversion <- latestVersion migrationtable database
  case targetversion of
    Target tver -> runTargetedMigrations currentversion tver migrations migrationtable database
    Latest      -> mapM (runMigration migrationtable Up database) $ filter (latestMigrationFilter currentversion) migrations

-- | Run migrations if we specified an actual version, this needs to consider whether or not the migration is up or down
runTargetedMigrations :: DatabaseHandler a
                      => MigrationVersion -- ^ Current version of the database
                      -> MigrationVersion -- ^ Target version of the database
                      -> [Migration]      -- ^ Available migrations
                      -> TableName        -- ^ Table that holds migration information
                      -> DBConnection a   -- ^ Active database connection
                      -> IO [Int]         -- ^ Number of rows affected per table
runTargetedMigrations currentversion targetversion migrations migrationtable database
  | targetversion > currentversion = mapM (runMigration migrationtable Up database) upMigrations
  | targetversion < currentversion = mapM (runMigration migrationtable Down database) downMigrations
  | otherwise                      = return []
  where
    upMigrations   = filter (boundedMigrateUpFilter targetversion currentversion) migrations
    downMigrations = reverse $ filter (boundedMigrateDownFilter targetversion currentversion) migrations

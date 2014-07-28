{-# LANGUAGE TypeFamilies #-}

-- | Data grouping for a database definition within an environment
module Yaskl.Data.Database where

import Database.PostgreSQL.Simple ( ConnectInfo(..)
                                  , Connection
                                  , Query
                                  , connect
                                  , defaultConnectInfo
                                  , execute
                                  , close              )

import Data.Yaml.Config           ( Config
                                  , Key    )

import Yaskl.Data.Migration       ( Direction
                                  , Migration
                                  , MigrationVersion )

-- | represent different supported data stores
data DataStore = PostgreSQL deriving (Show, Read)

-- | Type alias to make function typing more descriptive
type TableName = String

-- | Class defining necissary type interface for handling database connections
-- THe inverse of DBConnection is used to insure injectivity of the relationship between
-- DatabaseHandler and DBConnection a. Could've use data families here instead but it
-- is inconvienent elsewhere in the code do to so.
class UnDBConnection (DBConnection a) ~ a => DatabaseHandler a where
  type DBConnection a
  type UnDBConnection b

  initializeConnection :: a -> IO (DBConnection a)
  closeConnection      :: DBConnection a -> IO ()
  loadFromConfig       :: Config -> IO a

  -- | function called to ensure migration table exists and is empty
  initializeMigrations :: TableName      -- ^ Name of table to store migrations in
                       -> DBConnection a -- ^ Active database connection
                       -> IO Int         -- ^ Number of items affected

  -- | Runs an up or down migration
  runMigration         :: TableName      -- ^ Current place where migration Data is stored
                       -> Direction      -- ^ Whether this is an up or down migration
                       -> DBConnection a -- ^ active database connection
                       -> Migration      -- ^ Actual migration data
                       -> IO Int         -- ^ Number of things affected

  -- | Returns the current revision of the datastore
  latestVersion        :: TableName           -- ^ Current place where migration data is stored
                       -> DBConnection a      -- ^ Active connection to data store
                       -> IO MigrationVersion -- ^ The current version of data in the datastore

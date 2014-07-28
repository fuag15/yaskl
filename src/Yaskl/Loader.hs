{-# LANGUAGE OverloadedStrings #-}

-- | Module to help load a project from the hosts directory into a 'Data.Yaml.Config' record
module Yaskl.Loader (loadConfiguration) where

import Data.Yaml.Config          as Y  ( Config
                                       , Key
                                       , keys
                                       , load
                                       , subconfig
                                       , lookup    )

import Yaskl.Util.Directory            ( buildMigrationPaths
                                       , listDirectory
                                       , listAbsoluteDirectory
                                       , extractVersion        )

import Yaskl.PostgreSQL.Database as PG (Database(..))
import Yaskl.Data.Config         as C  (Config(..), Environment(..))
import Yaskl.Data.Migration            (Migration(..), MigrationVersion)
import Yaskl.Data.Database             (DatabaseHandler(..), DataStore)
import System.FilePath                 ((</>), takeFileName)
import Control.Monad                   (zipWithM, liftM)

-- | given the base directory of a project return a config representing the directory
loadConfiguration :: String -> IO (C.Config PG.Database)
loadConfiguration basename = do
  configuration  <- Y.load ( basename </> "config.yaml" )
  migrationpaths <- buildMigrationPaths basename
  migrationtable <- Y.lookup "migrations table" configuration
  datastore      <- liftM read (Y.lookup "data store" configuration)
  environments   <- loadEnvironments datastore =<< Y.load ( basename </> "environments.yaml")
  seed           <- loadSeedMigration          =<< Y.load ( basename </> "seed.yaml")
  bases          <- mapM baseMigrationLoader $ filter ((==) "base.yaml" . takeFileName) migrationpaths
  migrations     <- mapM migrationLoader     $ filter ((/=) "base.yaml" . takeFileName) migrationpaths
  return C.Config { environments   = environments
                  , migrationTable = migrationtable
                  , dataStore      = datastore
                  , seed           = seed
                  , bases          = bases
                  , migrations     = migrations }

-- | run through environments.yaml and load each top level environment definition
loadEnvironments :: DataStore                    -- ^ The backend class of database which defines the layout of a config
                 -> Y.Config                     -- ^ The yaml configuration file
                 -> IO [Environment PG.Database] -- ^ The Loaded environment configurations with associated Database Configurations
loadEnvironments datastore envconfig = do
  let environmentlist = keys envconfig
  environmentconfs <- mapM (`subconfig` envconfig) environmentlist
  zipWithM (loadEnvironment datastore) environmentconfs environmentlist

-- | load a single environment for accumulation in loadEnvironments
loadEnvironment :: DataStore                    -- ^ The backend class of database which defines the Environment config
                -> Y.Config                     -- ^ Yaml based environment config
                -> Key                          -- ^ The specific environment to load, this is a top level yaml key from the config
                -> IO (Environment PG.Database) -- ^ A loaded and verified Database configuration associated with an environment
loadEnvironment datastore envconfig envname = do
  database <- loadFromConfig envconfig :: IO PG.Database
  return Environment { C.name    = show envname
                     , databases = [database] }

-- | load the seed migration from seed.yaml
loadSeedMigration :: Y.Config -> IO Migration
loadSeedMigration seedconfig = do
  up <- Y.lookup "seed" seedconfig
  return Migration { version     = (0, 0)
                   , description = "Seed table"
                   , up          = up
                   , down        = "" }

-- | load all migrations into a list
migrationLoader :: FilePath -> IO Migration
migrationLoader migrationpath = do
  let versionnumber = extractVersion migrationpath
  migrationconfig <- Y.load migrationpath
  loadMigration versionnumber migrationconfig

-- | load all major version base migrations into a list
baseMigrationLoader :: FilePath -> IO Migration
baseMigrationLoader migrationpath = do
  let versionnumber = extractVersion migrationpath
  migrationconfig <- Y.load migrationpath
  loadBaseMigration versionnumber migrationconfig

-- | load a single migration yaml into a Migration
loadMigration :: MigrationVersion -- ^ The version number of the migration to load
              -> Y.Config         -- ^ The yaml migration file
              -> IO Migration     -- ^ A loaded and sanity checked migration definition
loadMigration version migrationconfig = do
  up          <- Y.lookup "up" migrationconfig
  down        <- Y.lookup "down" migrationconfig
  description <- Y.lookup "description" migrationconfig
  return Migration { version     = version
                   , description = description
                   , up          = up
                   , down        = down }

-- | load a single base.yaml into a Migration
loadBaseMigration :: MigrationVersion -- ^ The version of this migration, will always be of the form X, 0 where X is a major version of the database
                  -> Y.Config         -- ^ A yaml configuration for this migration, for base migrations this is a schema definition
                  -> IO Migration     -- ^ A loaded and sanity checked migration which will take the database from 0.0 to this major verison
loadBaseMigration version baseconfig = do
  up <- Y.lookup "schema" baseconfig
  return Migration { version     = version
                   , description = "base"
                   , up          = up
                   , down        = "" }

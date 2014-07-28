{-# LANGUAGE TypeFamilies, OverloadedStrings #-}

-- | Data grouping for a database definition within an environment
module Yaskl.PostgreSQL.Database where

import Yaskl.Data.Migration       as M ( Migration(..)
                                       , Direction(..)
                                       , MigrationVersion )

import Database.PostgreSQL.Simple      ( ConnectInfo(..)
                                       , Connection
                                       , Query
                                       , query
                                       , connect
                                       , defaultConnectInfo
                                       , withTransaction
                                       , execute
                                       , execute_
                                       , close              )

import Yaskl.Data.Database             ( DatabaseHandler(..)
                                       , TableName )

import Data.Yaml.Config           as Y (Config, lookup)
import Data.Int                        (Int64)
import Data.Word                       (Word16)
import Control.Monad                   (liftM)

-- | Grouping for all data related to connecting to a database
data Database = Database { name     :: String
                         , port     :: Int
                         , user     :: String
                         , password :: String
                         , host     :: String
                         } deriving (Show)

-- | Make this an instances of the Type Family DatabaseHandler
instance DatabaseHandler Database where
  type DBConnection   Database   = Connection
  type UnDBConnection Connection = Database
  initializeConnection           = getConnection
  closeConnection                = close
  initializeMigrations           = createMigrationTable
  loadFromConfig                 = loadDatabaseFromConfig
  runMigration                   = runOneMigration
  latestVersion                  = getLatestVersion

-- | load a database definition from an environment
loadDatabaseFromConfig :: Y.Config -> IO Database
loadDatabaseFromConfig dbconfig = do
  name     <- Y.lookup "name"     dbconfig
  user     <- Y.lookup "user"     dbconfig
  password <- Y.lookup "password" dbconfig
  port     <- Y.lookup "port"     dbconfig
  host     <- Y.lookup "host"     dbconfig
  return Database { name     = name
                  , port     = port
                  , user     = user
                  , password = password
                  , host     = host }

-- | Takes a loaded database configuration and returns an active connection to that db
getConnection :: Database -> IO Connection
getConnection databaseconfig = connect defaultConnectInfo { connectHost     = host databaseconfig
                                                          , connectPort     = fromIntegral $ port databaseconfig :: Word16
                                                          , connectUser     = user databaseconfig
                                                          , connectPassword = password databaseconfig
                                                          , connectDatabase = name databaseconfig }

-- | Initializes the migration table of a database
createMigrationTable :: TableName  -- ^ The name of the table to store migrations
                     -> Connection -- ^ The open connection to the database
                     -> IO Int     -- ^ An integer representing the number of affected tables
createMigrationTable migrationtable database = liftM fromIntegral $ execute database migrationTableSQL [migrationtable]
  where migrationTableSQL = read $ unlines [ "CREATE TABLE IF NOT EXISTS '?' ("
                                           , "'id' SERIAL PRIMARY KEY,"
                                           , "'description' varchar(30) DEFAULT NULL,"
                                           , "'major' integer,"
                                           , "'minor' integer);" ] :: Query

-- | Run a single migration against the database
runOneMigration :: TableName  -- ^ The table where migration information is stored
                -> Direction  -- ^ Is this an upgrade or downgrade
                -> Connection -- ^ Active database connection
                -> Migration  -- ^ Loaded migration definition
                -> IO Int     -- ^ The number of affected rows
runOneMigration migrationtable direction database migration = liftM fromIntegral $ withTransaction database migrate
  where (major, minor) = M.version migration
        migrate        = case direction of Up   -> do
                                             execute_ database (read $ up migration :: Query)
                                             execute database "insert into ? (major, minor, description) values (?, ?, ?);" (migrationtable, major, minor, description migration)
                                           Down -> do
                                             execute_ database (read $ down migration :: Query)
                                             execute database "delete from ? where major == ? and minor == ?);" (migrationtable, major, minor)

-- | Returns the current Version of the Database
getLatestVersion :: TableName           -- ^ The table where migration information is stored
                 -> Connection          -- ^ Active database handle
                 -> IO MigrationVersion -- ^ The current version of the database
getLatestVersion migrationtable database = liftM maximum (query database "select major, minor from ?;" [migrationtable] :: IO [MigrationVersion])

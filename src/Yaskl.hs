{-|
  This section of code loads yaml based configuration files describing a set of migration History on a Postgres
  database And either creates the Migration Table to record the history of migrations, Creates the structure
  off of a base of a Major revision, Or migrates any revision to any other give a Migration Version.

  Arguments:

  > yaskl project_root action environment target_version

  Where @target_version@ is optional and in the form of

  > (Major,Minor) :: (Int,Int)

  @environment@ is a defined environment in your project and @action@ is one of

      * Create - create the migrations table and run a base migrations of a version

      * Migrate - migrate up or down to the target database state

      * Seed - run the seed migration to repopulate the database

  If @target_version@ is not supplied it is assumed you want to migrate to the latest version

  Project Structure:

  > project_root
  > |-- 0 major version)
  > |   |-- base.yaml (major version base schema)
  > |   |-- 1.yaml (first minor revision migration)
  > |   `-- #.yaml ...
  > |-- 1 ... (as many major revisions as wanted, each should have a base schema)
  > |-- config.yaml (list migration table and type of database)
  > |-- environments.yaml (lists clusters of databases and connection info)
  > `-- seed.yaml (some seed data for the databases)

  Where the base schemas are designed to migrate from nothing to the current schema at the start of the major revision

  Project File Format:

  > base.yaml_
  >
  >    schema: sql

  > #.yaml
  >
  >    up: sql
  >    down: sql
  >    description: string (this will be stored in the migrations table for reference)

  > config.yaml
  >
  >    migrations table: valid_table_name (will be used to stor a version / migration info)
  >    data store: valid_datastore_descriptor (PostgreSQL)

  > seed.yaml
  >
  >    seed: sql

  > environments.yaml
  >
  >    environment_name:
  >      host:     string  /address of db/
  >      name:     string  /name of db/
  >      user:     string  /name of db admin account/
  >      password: string  /password of db admin/
  >      port:     integer /port that db listens on/

  where sql is valid sql statements, string is a string that is reasonable for the field etc.

  Error Strategy:

  On any sort of bad Input or migration trouble it will fail gracefully with a meaningfull error message.
-}
module Main where

import Yaskl.Data.Arguments    (projectDirectory)
import Yaskl.Loader            (loadConfiguration)
import Yaskl.Dispatcher        (buildArgs)
import Yaskl.Migrator          (dispatchTasks)
import System.Environment      (getArgs)

-- | parses args and load config from directory structure then dispatches your command
main :: IO ()
main = do
  args   <- buildArgs =<< getArgs
  config <- loadConfiguration $ projectDirectory args
  dispatchTasks args config

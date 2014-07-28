-- | Module grouping for data related to migrations
module Yaskl.Data.Migration where

-- | type alias to make clearer migration versions
type Major = Int

-- | type alias to make clearer migration versions
type Minor = Int

-- | Represent a version as a Major and Minor version tuple
type MigrationVersion = (Major, Minor)

-- | Helper Enumerator for running Migrations
data Direction = Up | Down deriving (Show)

-- | Definition of everything related to a migration
data Migration = Migration { version     :: MigrationVersion -- ^ What is the version of this migration
                           , description :: String           -- ^ A short description for record keeping
                           , up          :: String           -- ^ actions to migrate from the last revision to this one
                           , down        :: String           -- ^ actions to migrate from this revision to the last one
                           } deriving (Show, Eq, Ord)


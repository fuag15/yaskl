-- | Helper module for filtering versions by version, this can be almost completely done
-- away with by making Migration an instance of Ord and converting MigrationVersion to
-- (Int, Int)
module Yaskl.Util.Migration where

import Yaskl.Data.Migration ( Migration
                            , MigrationVersion
                            , version          )

-- | Filters away any version below the specified
latestMigrationFilter :: MigrationVersion -- ^ Target Version
                      -> Migration        -- ^ Migration data to be tested
                      -> Bool             -- ^ whether it was below the target version
latestMigrationFilter current = (> current) . version

-- | filters all less than currant and more than the target
-- this is for when you want to upgrade a Database but not to latest
boundedMigrateUpFilter :: MigrationVersion -- ^ Lowest bound for filter, (current version)
                       -> MigrationVersion -- ^ Highest bound for filter, (target version)
                       -> Migration        -- ^ Migration data to be tested
                       -> Bool             -- ^ Whether this migration is mor than the current version and less than the target
boundedMigrateUpFilter lower upper = and . sequence [(> lower) . version, (<= upper) . version]

-- | filters all greater than current and less than target
-- this is for when you want to downgrade a Database
boundedMigrateDownFilter :: MigrationVersion -- ^ Lowest bound for filter, (target version)
                         -> MigrationVersion -- ^ Highest bound for filter, (current version)
                         -> Migration        -- ^ Migration data to be tested
                         -> Bool             -- ^ Whether the data was less than the current version but more than the target
boundedMigrateDownFilter lower upper = and . sequence [(>= lower) . version, (< upper) . version]


-- | Data grouping for Argument related structures
module Yaskl.Data.Arguments where

import Yaskl.Data.Migration (MigrationVersion)

-- | Target environment name to match for in @environments.yaml@
-- this is defined to allow for more descriptive function Types
type Environment = String

-- | Base directory for Project configuration structure
type ProjectDirectory = String

-- | Either a Float of a target version of Latest to specify intent in a matchable way
data Version = Latest | Target MigrationVersion deriving (Read, Show)

-- | Enumerated list of actions to match against
data Action = Create | Migrate | Seed deriving (Read, Show)

-- | Record structure to hold all Arguments for a single run of Yaskl
data Arguments = Arguments { version          :: Version          -- ^ Target version of this run
                           , projectDirectory :: ProjectDirectory -- ^ Directory containing data store configuration and migrations
                           , action           :: Action           -- ^ Upgrade or downgrade
                           , environment      :: Environment      -- ^ What envrionment to run this against
                           } deriving (Show)


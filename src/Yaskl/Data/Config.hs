{-# LANGUAGE RankNTypes #-}

-- | Data grouping for fields related to a Full directory configuration structure
module Yaskl.Data.Config where

import Yaskl.Data.Database  ( TableName
                            , DatabaseHandler
                            , DataStore       )

import Yaskl.Data.Migration (Migration)

-- | grouped environments
data Environment a = Environment { name      :: String
                                 , databases :: [a] }

-- | group the whole project config into a structure
data Config a = Config { environments   :: [Environment a]
                       , migrationTable ::  TableName
                       , dataStore      ::  DataStore
                       , seed           ::  Migration
                       , bases          :: [Migration]
                       , migrations     :: [Migration] }

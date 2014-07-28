-- | Module to Parse command line arguments and return a 'Yaskl.Data.Arguments' record
module Yaskl.Dispatcher (buildArgs) where

import Yaskl.Data.Arguments ( Arguments(..)
                            , Version(..)
                            , Action(..)
                            , Environment
                            , ProjectDirectory )

import Control.Monad        (when)

-- | External function to kick off argument parsing
buildArgs :: [String] -> IO Arguments
buildArgs args = do
  when (length args < 3) $ fail "Too few Arguments"
  case parseArgs args of
    Just builtargs -> return builtargs
    Nothing        -> fail "Invalad Arguments"

-- | wrapper function to do the smart thing with Maybe monads
parseArgs :: [String] -> Maybe Arguments
parseArgs args = do
  action'          <- parseAction args
  version'         <- parseVersion args
  environment'     <- parseEnvironment args
  projectdirectory <- parseProjectDirectory args
  return Arguments { action           = action'
                   , projectDirectory = projectdirectory
                   , version          = version'
                   , environment      = environment' }

-- | Returns either a valid action or Nothing
parseAction :: [String] -> Maybe Action
parseAction args
  | intent == "create"  = Just Create
  | intent == "migrate" = Just Migrate
  | intent == "seed"    = Just Seed
  | otherwise           = Nothing
  where intent = args !! 1

-- | returns a directory from a valid argument lists
parseProjectDirectory :: [String] -> Maybe ProjectDirectory
parseProjectDirectory = extractDirectory
  where extractDirectory [ dir, _, _, _] = Just dir
        extractDirectory [ dir, _, _]    = Just dir
        extractDirectory   _             = Nothing

-- | returns an environment string from a valid argument list
parseEnvironment :: [String] -> Maybe Environment
parseEnvironment = extractEnv
  where extractEnv [ _, _, env, _ ] = Just env
        extractEnv [ _, _, env ]    = Just env
        extractEnv   _              = Nothing


-- | returns a valid version from a valid argument list or Latest if one isn't specified
parseVersion :: [String] -> Maybe Version
parseVersion = extractVersion
  where extractVersion [ _, _, _, ver] = Just $ Target $ read ver
        extractVersion [ _, _, _]      = Just Latest
        extractVersion   _             = Nothing

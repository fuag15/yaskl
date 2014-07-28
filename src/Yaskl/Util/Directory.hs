-- | Utility to pull info out of directories, this can be greatly simplified by using
-- 'System.Directory.Tree' and this should be refactored in the future
module Yaskl.Util.Directory where

import System.Directory     ( doesDirectoryExist
                            , getDirectoryContents )

import System.FilePath      ( combine
                            , splitDirectories
                            , dropFileName
                            , takeBaseName     )

import Control.Monad        (filterM, liftM)
import Control.Applicative  ((<$>))
import Yaskl.Data.Migration (MigrationVersion)

-- | Utility to build a full migration path from the start of a base directory
buildMigrationPaths :: String -> IO [FilePath]
buildMigrationPaths basename = do
  migrationversiondirectories <- filterM doesDirectoryExist =<< listAbsoluteDirectory basename
  liftM concat $ mapM listAbsoluteDirectory migrationversiondirectories

-- | lists a directory contents removing the . / .. from the returned FilPath list
listDirectory :: String -> IO [FilePath]
listDirectory directory = filter (`notElem` [".", ".."]) <$> getDirectoryContents directory

-- | Lists a directory while maintaint the basename
listAbsoluteDirectory :: FilePath -> IO [FilePath]
listAbsoluteDirectory basedir = liftM (fmap $ combine basedir) $ listDirectory basedir

-- | returns a parsed version from a directory path from a numbered migrations
extractVersion :: FilePath -> MigrationVersion
extractVersion fullpath
  | minorVersion == "base" = (majorVersion, 0)
  | otherwise              = (majorVersion, read minorVersion)
  where minorVersion = takeBaseName fullpath
        majorVersion = read $ last $ splitDirectories $ dropFileName fullpath

-- | This is just a placeholder function to keep the example of different types of unit tests
sayHi :: String -> String
sayHi name = "Hello mr. " ++ name

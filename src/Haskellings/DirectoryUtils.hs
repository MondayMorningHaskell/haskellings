{-|
Module      : Haskellings.DirectoryUtils
Description : Functions for manipulating file paths and directories.
License     : BSD3
Maintainer  : james@mondaymorninghaskell.me

This module has constant values that are used across Haskellings.
It includes things like the string for the GHC version currently in use,
hard-coded directory and file names, and required libraries.
-}

{-# LANGUAGE FlexibleContexts #-}

module Haskellings.DirectoryUtils (
  -- * File Path Manipulation
  haskellModuleName,
  haskellFileName,
  -- * File and Directory Checks
  fileContainsNotDone,
  returnIfDirExists,
  fullExerciseFp,
  safeListDirectory,
  -- * Wrapping IO Actions
  withFileLock,
  withDirectory,
  -- * Searching for Directories
  searchForDirectoryContaining,
  fpBFS,
  -- * OS Utilities
  isWindows
) where

import           Control.Concurrent
import           Control.Exception          (catch)
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Data.Char
import           Data.List.Extra            (upper)
import qualified Data.Map                   as M
import qualified Data.Sequence              as S
import           System.Directory
import           System.FilePath            (takeBaseName, takeFileName, (</>))
import           System.Info                (os)

import           Haskellings.Types

-- | Get the Haskell module name for a file, so we can compare it to an
--   exercise name. (e.g. "exercises/Expressions.hs" -> "Expressions"
haskellModuleName :: FilePath -> FilePath
haskellModuleName = takeBaseName

-- | Appends the ".hs" extension to a file.
haskellFileName :: FilePath -> FilePath
haskellFileName exName = exName ++ ".hs"

-- | Determines if a file contains the string "I AM NOT DONE" on a line.
--   If this line exists, the Watcher won't move on to the next exercise.
fileContainsNotDone :: FilePath -> IO Bool
fileContainsNotDone fullFp = do
  fileLines <- lines <$> readFile fullFp
  return (any isDoneLine fileLines)
  where
    isDoneLine :: String -> Bool
    isDoneLine l = (upper . filter (not . isSpace) $ l) == "--IAMNOTDONE"

-- | If the (absolute) path exists and is a directory, return it.
--   Otherwise, return Nothing.
returnIfDirExists :: FilePath -> IO (Maybe FilePath)
returnIfDirExists fp = do
  exists <- doesDirectoryExist fp
  if exists
    then return (Just fp)
    else return Nothing

-- | Construct a full filepath leading to the source file of an 'ExerciseInfo'
fullExerciseFp :: FilePath     -- ^ Absolute path to the project root
               -> FilePath     -- ^ Extension to the base 'exercises' directory
               -> ExerciseInfo -- ^ The ExerciseInfo object
               -> FilePath     -- ^ Absolute path to the exercise file
fullExerciseFp projectRoot exercisesExt (ExerciseInfo exName exDir _ _) =
  projectRoot </> exercisesExt </> exDir </> haskellFileName exName

-- | Works like System.Directory.listDirectory, but returns an empty list if there's a
--   IOError, like a permission error (this occurs on Windows).
safeListDirectory :: FilePath -> IO [FilePath]
safeListDirectory fp = catch (listDirectory fp) emptyListHandler
  where
    emptyListHandler :: IOError -> IO [FilePath]
    emptyListHandler = const (return [])

-- | Locks a file before performing an action with it.
--   Requires Reader access to a Program Config, which
--   contains the map of locks.
--   If a file is not in the map, no locking occurs.
--   This is primarily used in testing.
withFileLock :: (MonadIO m, MonadReader ProgramConfig m)
  => FilePath -- ^ Absolute path to file you want to lock
  -> m a      -- ^ Action to take with the file lock
  -> m a      -- ^ Final wrapped action
withFileLock fp action = do
  maybeLock <- M.lookup fp <$> asks fileLocks
  case maybeLock of
    Nothing -> action
    Just lock -> do
      liftIO $ putMVar lock ()
      result <- action
      liftIO $ takeMVar lock
      return result

-- | Create a directory. Run the action depending on that directory,
--   and then clean the directory up, removing it from the file system.
--   Used to clean up compile artifacts from running an exercise.
withDirectory :: (MonadIO m) => FilePath -> m a -> m a
withDirectory dirPath action = do
  liftIO $ createDirectoryIfMissing True dirPath
  res <- action
  liftIO $ removeDirectoryRecursive dirPath
  return res

-- | Find a directory under the search root matching the given name.
searchForDirectoryContaining :: FilePath            -- ^ The (absolute) path to search from.
                             -> String              -- ^ The (relative) directory name to find.
                             -> IO (Maybe FilePath) -- ^ The (absolute) path if it is found, or Nothing if not.
searchForDirectoryContaining searchRoot directoryToFind = fpBFS predicate (S.singleton searchRoot)
  where
    predicate fp = do
      isDirectory <- doesDirectoryExist fp
      return $ isDirectory && directoryToFind == takeFileName fp

-- | Performs a Bread-First-Search (BFS) to locate a directory or filepath
--   matching a particular predicate.
--   Works tail recursively, tracking the current search queue as a Seq.
fpBFS :: (FilePath -> IO Bool) -- ^ A predicate function, indicating if a filepath is satisfactory.
      -> S.Seq FilePath        -- ^ The existing queue of directories to search (used recursively).
      -> IO (Maybe FilePath)   -- ^ The (absolute) path if it is found, or Nothing if not.
fpBFS predicate searchQueue = case S.viewl searchQueue of
  S.EmptyL -> return Nothing
  (currentRoot S.:< rest) -> do
    fpSatisfiesPred <- predicate currentRoot
    if fpSatisfiesPred
      then return (Just currentRoot)
      else do
        currentIsDir <- doesDirectoryExist currentRoot
        if currentIsDir
          then do
            contents <- safeListDirectory currentRoot
            let results = map (currentRoot </>) contents
            fpBFS predicate $ rest S.>< S.fromList results
          else fpBFS predicate rest

-- | Determine if we are running on Windows or a POSIX based system.
--   This affects where GHC and Stack packages are located.
isWindows :: Bool
isWindows = os `notElem` ["linux", "unix", "darwin"]

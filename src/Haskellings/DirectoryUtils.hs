{- Utility functions for manipulating filepaths and directories. -}

{-# LANGUAGE FlexibleContexts #-}

module Haskellings.DirectoryUtils where

import           Control.Concurrent
import           Control.Exception          (catch)
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Data.Char
import           Data.List                  (isSuffixOf)
import           Data.List.Extra            (upper)
import qualified Data.Map                   as M
import qualified Data.Sequence              as S
import           System.Directory
import           System.FilePath            (takeBaseName, takeFileName, (</>))
import           System.Info                (os)

import           Haskellings.Types

isWindows :: Bool
isWindows = os `notElem` ["linux", "unix", "darwin"]

isHaskellFile :: FilePath -> Bool
isHaskellFile = isSuffixOf ".hs"

-- Probably a good idea to first check that it is a Haskell file first
haskellModuleName :: FilePath -> FilePath
haskellModuleName = takeBaseName

haskellFileName :: FilePath -> FilePath
haskellFileName exName = exName ++ ".hs"

fileContainsNotDone :: FilePath -> IO Bool
fileContainsNotDone fullFp = do
  fileLines <- lines <$> readFile fullFp
  return (any isDoneLine fileLines)
  where
    isDoneLine :: String -> Bool
    isDoneLine l = (upper . filter (not . isSpace) $ l) == "--IAMNOTDONE"

fullExerciseFp :: FilePath -> FilePath -> ExerciseInfo -> FilePath
fullExerciseFp projectRoot exercisesExt (ExerciseInfo exName exDir _ _) =
  projectRoot </> exercisesExt </> exDir </> haskellFileName exName

withFileLock :: (MonadIO m, MonadReader ProgramConfig m) => FilePath -> m a -> m a
withFileLock fp action = do
  maybeLock <- M.lookup fp <$> asks fileLocks
  case maybeLock of
    Nothing -> action
    Just lock -> do
      liftIO $ putMVar lock ()
      result <- action
      liftIO $ takeMVar lock
      return result

-- Create a directory. Run the action depending on that directory,
-- and then clean the directory up.
withDirectory :: (MonadIO m) => FilePath -> m a -> m a
withDirectory dirPath action = do
  liftIO $ createDirectoryIfMissing True dirPath
  res <- action
  liftIO $ removeDirectoryRecursive dirPath
  return res

returnIfDirExists :: FilePath -> IO (Maybe FilePath)
returnIfDirExists fp = do
  exists <- doesDirectoryExist fp
  if exists
    then return (Just fp)
    else return Nothing

---------- Directory Search Functions ----------

searchForDirectoryContaining :: FilePath -> String -> IO (Maybe FilePath)
searchForDirectoryContaining searchRoot directoryToFind = fpBFS predicate (S.singleton searchRoot)
  where
    predicate fp = do
      isDirectory <- doesDirectoryExist fp
      return $ isDirectory && directoryToFind == takeFileName fp

-- Given a "file predicate" function
fpBFS :: (FilePath -> IO Bool) -> S.Seq FilePath -> IO (Maybe FilePath)
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

-- Like 'listDirectory', but returns an empty list if there's a
-- permission error.
safeListDirectory :: FilePath -> IO [FilePath]
safeListDirectory fp = catch (listDirectory fp) emptyListHandler
  where
    emptyListHandler :: IOError -> IO [FilePath]
    emptyListHandler = const (return [])

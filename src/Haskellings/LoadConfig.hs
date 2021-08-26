{-|
Module      : Haskellings.LoadConfig
Description : Functions for loading configuration information
License     : BSD3
Maintainer  : james@mondaymorninghaskell.me

This module locates key elements of the project configuration, such
as the project root, GHC executable path, and Stack package DB location.
-}

{-# LANGUAGE OverloadedStrings #-}

module Haskellings.LoadConfig (
  -- * Project Configuration Functions
  loadBaseConfigPaths,
  findProjectRoot,
  -- * Predicates for GHC/Stack paths
  ghcPred,
  snapshotPackagePredicate
) where

import           Control.Monad              (forM)
import           Data.List                  (find, isPrefixOf, isSuffixOf)
import           Data.Maybe                 (catMaybes)
import qualified Data.Sequence              as S
import           Data.Yaml                  (decodeFileEither)
import           System.Directory
import           System.Environment         (lookupEnv)
import           System.FilePath            (takeDirectory, takeFileName, (</>))

import           Haskellings.Constants
import           Haskellings.DirectoryUtils
import           Haskellings.Types

-- | Locates 3 paths: The Project root path, the GHC executable path,
--   and the Stack package DB path.
--   If these cannot be found, an appropriate ConfigError is returned instead.
loadBaseConfigPaths :: IO (Either ConfigError (FilePath, FilePath, FilePath))
loadBaseConfigPaths = do
  projectRoot' <- findProjectRoot
  case projectRoot' of
    Nothing          -> return (Left NoProjectRootError)
    Just projectRoot -> loadBaseConfigPathsWithProjectRoot projectRoot

-- | Locates the project root (e.g. 'haskellings' directory) via
--   a Breadth-First-Search from the "home" directory.
findProjectRoot :: IO (Maybe FilePath)
findProjectRoot = do
  home <- getHomeDirectory
  isCi <- envIsCi
  if isCi
    then searchForDirectoryContaining home ciProjectRootDirName
    else searchForDirectoryContaining home projectRootDirName

---------- EXPORTED ONLY FOR TESTING ----------

-- | A predicate to determine if a directory is a valid "ghc" directory.
--   It must start with "ghc" and end with our version number.
ghcPred :: FilePath -> Bool
ghcPred path = isPrefixOf "ghc" (takeFileName path) && isSuffixOf ghcVersionNumber path

-- | A predicated to determine if a directory can contains an appropriate
--   Stack snapshot we can use.
snapshotPackagePredicate :: FilePath -> IO Bool
snapshotPackagePredicate fp = if not (ghcVersion `isSuffixOf` fp)
  then return False
  else do
    let fullDir = pkgPathFromGhcPath fp
    contents <- safeListDirectory fullDir
    -- Each required library must have a subpackage directory in the package DB.
    let containsLib lib = any (isPrefixOf lib) contents
    return $ all containsLib haskellingsRequiredLibs

---------- PRIVATE FUNCTIONS ----------

loadBaseConfigPathsWithProjectRoot :: FilePath -> IO (Either ConfigError (FilePath, FilePath, FilePath))
loadBaseConfigPathsWithProjectRoot projectRoot = do
  let configPath = projectRoot </> configFileName
  configExists <- doesFileExist configPath
  baseConfig <- if configExists
    then do
      fileResult <- decodeFileEither configPath
      case fileResult of
        (Left _)       -> return (BaseConfig Nothing Nothing)
        (Right config) -> return config
    else return (BaseConfig Nothing Nothing)
  ghcPath' <- case baseConfigGhcPath baseConfig of
    Nothing -> findGhc
    Just p  -> return (Just p)
  stackPath' <- case baseConfigStackPath baseConfig of
    Nothing -> findStackPackageDb
    Just p  -> return (Just p)
  case (ghcPath', stackPath') of
    (Just ghcPath, Just stackPath) -> return (Right (projectRoot, ghcPath, stackPath))
    (Just _, Nothing)        -> return (Left NoStackPackageDbError)
    (Nothing, _)             -> return (Left NoGhcError)

findGhc :: IO (Maybe FilePath)
findGhc = do
  home <- getHomeDirectory
  ghcSearchDir' <- findGhcSearchDir
  case ghcSearchDir' of
    Nothing -> return Nothing
    Just ghcSearchDir -> do
      nextDirs <- listDirectory ghcSearchDir
      results <- forM nextDirs $ \subPath -> do
        let fullPath = ghcSearchDir </> subPath
        subContents <- safeListDirectory fullPath
        return $ fmap (fullPath </>) (find ghcPred subContents)
      case catMaybes results of
        []       -> return Nothing
        (fp : _) -> return $ Just (fp </> "bin" </> "ghc")

findStackPackageDb :: IO (Maybe FilePath)
findStackPackageDb = do
  home <- getHomeDirectory
  stackDir' <- findStackSnapshotsDir
  case stackDir' of
    Nothing -> return Nothing
    Just stackDir -> do
      ghcVersionDir' <- fpBFS snapshotPackagePredicate (S.singleton stackDir)
      case ghcVersionDir' of
        Nothing -> return Nothing
        Just ghcVersionDir -> return $ Just (pkgPathFromGhcPath ghcVersionDir)

-- The GHC version path might look like {hash}/8.10.4/lib/x86_64-linux-ghc-8.10.4
-- We want to get the package path, at {hash}/8.10.4/pkgdb
pkgPathFromGhcPath :: FilePath -> FilePath
pkgPathFromGhcPath ghcVersionDir = takeDirectory (takeDirectory ghcVersionDir) </> "pkgdb"

findStackSnapshotsDir :: IO (Maybe FilePath)
findStackSnapshotsDir = if isWindows
  then findStackSnapshotsDirWindows
  else findStackSnapshotsDirUnix

findStackSnapshotsDirUnix :: IO (Maybe FilePath)
findStackSnapshotsDirUnix = do
  homeDir <- getHomeDirectory
  let dir = homeDir </> ".stack" </> "snapshots"
  returnIfDirExists dir

findStackSnapshotsDirWindows :: IO (Maybe FilePath)
findStackSnapshotsDirWindows = do
  dir' <- lookupEnv "STACK_ROOT"
  case dir' of
    Nothing  -> return Nothing
    Just dir -> returnIfDirExists (dir </> "snapshots")

findGhcSearchDir :: IO (Maybe FilePath)
findGhcSearchDir = if isWindows
  then findGhcSearchDirWindows
  else findGhcSearchDirUnix

findGhcSearchDirUnix :: IO (Maybe FilePath)
findGhcSearchDirUnix = do
  isCi <- envIsCi
  homeDir <- if isCi
    -- Unintuitively, "/home" is not the same as "~" on Circle CI
    then return ("/home" </> "stackage")
    else getHomeDirectory
  let dir = homeDir </> ".stack" </> "programs"
  returnIfDirExists dir

findGhcSearchDirWindows :: IO (Maybe FilePath)
findGhcSearchDirWindows = do
  localAppDataDir' <- lookupEnv "LOCALAPPDATA"
  case localAppDataDir' of
    Nothing -> return Nothing
    Just localAppDataDir -> do
      let dir = localAppDataDir </> "Programs" </> "stack"
      returnIfDirExists dir

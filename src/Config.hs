{-# LANGUAGE OverloadedStrings #-}

module Config where


import           Control.Concurrent  (MVar, putMVar, takeMVar)
import           Control.Monad       (forM)
import           Data.Aeson
import           Data.List           (all, any, find, isPrefixOf, isSuffixOf)
import qualified Data.Map            as M
import           Data.Maybe          (catMaybes, isJust)
import qualified Data.Sequence       as S
import           Data.Yaml           (decodeFileEither)
import           System.Console.ANSI
import           System.Directory
import           System.Environment  (lookupEnv)
import           System.IO

import           DirectoryUtils

ghcVersion :: String
ghcVersion = "ghc-8.8.4"

ghcVersionNumber :: String
ghcVersionNumber = "8.8.4"

projectRootDirName :: String
projectRootDirName = "haskellings"

configFileName :: String
configFileName = "config.yaml"

-- On CircleCI, the root directory shows up as "project'
ciEnvName :: String
ciEnvName = "HASKELLINGS_CI_ENV"

envIsCi :: IO Bool
envIsCi = isJust <$> lookupEnv ciEnvName

ciProjectRootDirName :: String
ciProjectRootDirName = "project"

haskellingsVersion :: String
haskellingsVersion = "0.8.0.0"

mainProjectExercisesDir :: String
mainProjectExercisesDir = makeRelative "exercises"

-- A listing of packages required by exercises, so we can use them
-- to filter Stack snapshots
requiredLibs :: [String]
requiredLibs =
  [ "tasty"
  , "tasty-hunit"
  ]

data ConfigError = NoProjectRootError | NoGhcError | NoStackPackageDbError
  deriving (Show)

type FileLockMap = M.Map FilePath (MVar ())

withFileLock :: FilePath -> ProgramConfig -> IO a -> IO a
withFileLock fp config action = case M.lookup fp (fileLocks config) of
  Nothing -> action
  Just lock -> do
    putMVar lock ()
    result <- action
    takeMVar lock
    return result

data BaseConfig = BaseConfig
  { baseConfigGhcPath   :: Maybe FilePath
  , baseConfigStackPath :: Maybe FilePath
  }

instance ToJSON BaseConfig where
  toJSON (BaseConfig ghc stackPackageDb) = object
    [ "ghc_path" .= ghc
    , "stack_package_db_path" .= stackPackageDb
    ]

instance FromJSON BaseConfig where
  parseJSON = withObject "BaseConfig" $ \o -> do
    ghc <- o .:? "ghc_path"
    stackPackageDb <- o .:? "stack_package_db_path"
    return $ BaseConfig ghc stackPackageDb

data ProgramConfig = ProgramConfig
  { projectRoot  :: FilePath
  , ghcPath      :: FilePath
  , packageDb    :: FilePath
  , exercisesExt :: FilePath
  , inHandle     :: Handle
  , outHandle    :: Handle
  , errHandle    :: Handle
  , fileLocks    :: FileLockMap
  }

progPutStr :: ProgramConfig -> String -> IO ()
progPutStr pc = hPutStr (outHandle pc)

progPutStrLn :: ProgramConfig -> String -> IO ()
progPutStrLn pc = hPutStrLn (outHandle pc)

progPrint :: (Show a) => ProgramConfig -> a -> IO ()
progPrint pc = hPrint (outHandle pc)

progPutStrErr :: ProgramConfig -> String -> IO ()
progPutStrErr pc = hPutStrLn (errHandle pc)

progPrintErr :: (Show a) => ProgramConfig -> a -> IO ()
progPrintErr pc = hPrint (errHandle pc)

progReadLine :: ProgramConfig -> IO String
progReadLine pc = hGetLine (inHandle pc)

-- Perform an action with 'Green' Terminal Text
withTerminalSuccess :: IO a -> IO a
withTerminalSuccess = withTerminalColor Green

-- Perform an action with 'Red' Terminal Text
withTerminalFailure :: IO a -> IO a
withTerminalFailure = withTerminalColor Red

-- Perform an action with printed output given a color.
withTerminalColor :: Color -> IO a -> IO a
withTerminalColor color action = do
  setSGR [SetColor Foreground Vivid color]
  res <- action
  setSGR [Reset]
  return res

-- Print a line, but in Green
progPutStrLnSuccess :: ProgramConfig -> String -> IO ()
progPutStrLnSuccess pc output = withTerminalSuccess (progPutStrLn pc output)

-- Print a line, but in Red
progPutStrLnFailure :: ProgramConfig -> String -> IO ()
progPutStrLnFailure pc output = withTerminalFailure (progPutStrLn pc output)

-- Create a directory. Run the action depending on that directory,
-- and then clean the directory up.
withDirectory :: FilePath -> IO a -> IO a
withDirectory dirPath action = do
  createDirectoryIfMissing True dirPath
  res <- action
  removeDirectoryRecursive dirPath
  return res

loadBaseConfigPaths :: IO (Either ConfigError (FilePath, FilePath, FilePath))
loadBaseConfigPaths = do
  projectRoot' <- findProjectRoot
  case projectRoot' of
    Nothing -> return (Left NoProjectRootError)
    Just projectRoot -> loadBaseConfigPathsWithProjectRoot projectRoot

loadBaseConfigPathsWithProjectRoot :: FilePath -> IO (Either ConfigError (FilePath, FilePath, FilePath))
loadBaseConfigPathsWithProjectRoot projectRoot = do
  let configPath = projectRoot `pathJoin` configFileName
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
        let fullPath = ghcSearchDir `pathJoin` subPath
        subContents <- safeListDirectory fullPath
        return $ fmap (pathJoin fullPath) (find (==ghcVersion) subContents)
      case catMaybes results of
        []       -> return Nothing
        (fp : _) -> return $ Just (fp `pathJoin` "bin" `pathJoin` "ghc")

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

-- The GHC version path might look like {hash}/8.8.4/lib/x86_64-linux-ghc-8.8.4
-- We want to get the package path, at {hash}/8.8.4/pkgdb
pkgPathFromGhcPath :: FilePath -> FilePath
pkgPathFromGhcPath ghcVersionDir = pathJoin (dropDirectoryLevel (dropDirectoryLevel ghcVersionDir)) "pkgdb"

snapshotPackagePredicate :: FilePath -> IO Bool
snapshotPackagePredicate fp = if not (ghcVersion `isSuffixOf` fp)
  then return False
  else do
    let fullDir = pkgPathFromGhcPath fp
    contents <- safeListDirectory fullDir
    -- Each required library must have a subpackage directory in the package DB.
    let containsLib lib = any (isPrefixOf lib) contents
    return $ all containsLib requiredLibs

-- BFS
findProjectRoot :: IO (Maybe FilePath)
findProjectRoot = do
  home <- getHomeDirectory
  isCi <- envIsCi
  if isCi
    then searchForDirectoryContaining home ciProjectRootDirName
    else searchForDirectoryContaining home projectRootDirName

findStackSnapshotsDir :: IO (Maybe FilePath)
findStackSnapshotsDir = if isWindows
  then findStackSnapshotsDirWindows
  else findStackSnapshotsDirUnix

findStackSnapshotsDirUnix :: IO (Maybe FilePath)
findStackSnapshotsDirUnix = do
  homeDir <- getHomeDirectory
  let dir = homeDir `pathJoin` ".stack" `pathJoin` "snapshots"
  returnIfDirExists dir

findStackSnapshotsDirWindows :: IO (Maybe FilePath)
findStackSnapshotsDirWindows = do
  dir' <- lookupEnv "STACK_ROOT"
  case dir' of
    Nothing  -> return Nothing
    Just dir -> returnIfDirExists (dir `pathJoin` "snapshots")

findGhcSearchDir :: IO (Maybe FilePath)
findGhcSearchDir = if isWindows
  then findGhcSearchDirWindows
  else findGhcSearchDirUnix

findGhcSearchDirUnix :: IO (Maybe FilePath)
findGhcSearchDirUnix = do
  isCi <- envIsCi
  homeDir <- if isCi
    -- Unintuitively, "/home" is not the same as "~" on Circle CI
    then return ("/home" `pathJoin` "stackage")
    else getHomeDirectory
  let dir = homeDir `pathJoin` ".stack" `pathJoin` "programs"
  returnIfDirExists dir

findGhcSearchDirWindows :: IO (Maybe FilePath)
findGhcSearchDirWindows = do
  localAppDataDir' <- lookupEnv "LOCALAPPDATA"
  case localAppDataDir' of
    Nothing -> return Nothing
    Just localAppDataDir -> do
      let dir = localAppDataDir `pathJoin` "Programs" `pathJoin` "stack"
      returnIfDirExists dir

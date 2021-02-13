module Config where

import Control.Monad (forM)
import Data.List (find, isSuffixOf, isInfixOf)
import Data.Maybe (catMaybes)
import qualified Data.Sequence as S
import System.Directory
import System.Environment (lookupEnv)
import System.IO

import DirectoryUtils

ghcVersion :: String
ghcVersion = "ghc-8.8.4"

ghcVersionNumber :: String
ghcVersionNumber = "8.8.4"

projectRootDirName :: String
projectRootDirName = "haskellings"

mainProjectExercisesDir :: String
mainProjectExercisesDir = if isWindows
  then "\\src\\exercises\\"
  else "/src/exercises/"

data ConfigError = NoProjectRootError | NoGhcError
  deriving (Show)

data ProgramConfig = ProgramConfig
  { projectRoot  :: FilePath
  , ghcPath      :: FilePath
  , packageDb    :: Maybe FilePath
  , exercisesExt :: FilePath
  , inHandle     :: Handle
  , outHandle    :: Handle
  , errHandle    :: Handle
  }

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

loadProjectRootAndGhc :: IO (Either ConfigError (FilePath, FilePath))
loadProjectRootAndGhc = do
  projectRoot' <- findProjectRoot
  ghcPath' <- findGhc
  case (projectRoot', ghcPath') of
    (Just projectRoot, Just ghcPath) -> return (Right (projectRoot, ghcPath))
    (Just _, Nothing) -> return (Left NoGhcError)
    (Nothing, _) -> return (Left NoProjectRootError)

findGhc :: IO (Maybe FilePath)
findGhc = do
  home <- getHomeDirectory
  ghcSearchDir' <- findGhcSearchDir
  case ghcSearchDir' of
    Nothing -> return Nothing
    Just ghcSearchDir -> do
      nextDirs <- safeListDirectory ghcSearchDir
      results <- forM nextDirs $ \subPath -> do
        let fullPath = pathJoin ghcSearchDir subPath
        subContents <- safeListDirectory fullPath
        return $ fmap (pathJoin fullPath) (find (== ghcVersion) subContents)
      case catMaybes results of
        [] -> return Nothing
        (fp : _) -> return $ Just (fp `pathJoin` "bin" `pathJoin` "ghc")

-- TODO: This doesn't necessarily account for having multiple snapshots that
--       both use 8.8.4. This logic definitely needs to be tighter.
findStackPackageDb :: IO (Maybe FilePath)
findStackPackageDb = do
  home <- getHomeDirectory
  stackDir' <- findStackSnapshotsDir
  case stackDir' of
    Nothing -> return Nothing
    Just stackDir -> do
      nextDirs <- safeListDirectory stackDir
      results <- forM nextDirs $ \subPath -> do
        -- 'fullPath' is like .stack/snapshots/linux_x86_64
        let fullPath = stackDir `pathJoin` subPath
        subContents <- safeListDirectory fullPath
        results' <- forM subContents $ \hashDirectory -> do
          -- 'fullHashPath' is like .stack/snapshots/linux_x86_64/77asdfasdf...
          let fullHashPath = fullPath `pathJoin` hashDirectory
          allGhcDirs <- safeListDirectory fullHashPath
          return $ fmap (pathJoin fullHashPath) (find (== ghcVersionNumber) allGhcDirs)
        return $ catMaybes results'
      case concat results of
        [] -> return Nothing
        (pkgPath : _) -> return (Just $ pathJoin pkgPath "pkgdb")

-- BFS
-- Assumes home is NOT the project
-- TODO: Use BFS helper
findProjectRoot :: IO (Maybe FilePath)
findProjectRoot = do
  home <- getHomeDirectory
  searchForDirectoryContaining home projectRootDirName

-- TODO: Allow custom configuration
findStackSnapshotsDir :: IO (Maybe FilePath)
findStackSnapshotsDir = if isWindows
  then findStackSnapshotsDirWindows
  else findStackSnapshotsDirUnix

findStackSnapshotsDirUnix :: IO (Maybe FilePath)
findStackSnapshotsDirUnix = do
  homeDir <- getHomeDirectory
  return $ Just (pathJoin homeDir ".stack/snapshots")

findStackSnapshotsDirWindows :: IO (Maybe FilePath)
findStackSnapshotsDirWindows = lookupEnv "STACK_ROOT"

-- TODO: Allow custom configuration
findGhcSearchDir :: IO (Maybe FilePath)
findGhcSearchDir = if isWindows
  then findGhcSearchDirWindows
  else findGhcSearchDirUnix

findGhcSearchDirUnix :: IO (Maybe FilePath)
findGhcSearchDirUnix = do
  homeDir <- getHomeDirectory
  return $ Just (pathJoin homeDir ".stack/programs")

findGhcSearchDirWindows :: IO (Maybe FilePath)
findGhcSearchDirWindows = do
  localAppDataDir <- lookupEnv "LOCALAPPDATA"
  return $ (`pathJoin` "Programs\\stack") <$> localAppDataDir

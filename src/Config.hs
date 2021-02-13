module Config where

import Control.Monad (forM)
import Data.List (find, isSuffixOf)
import Data.Maybe (catMaybes)
import qualified Data.Sequence as S
import System.Directory
import System.IO

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
  let stackDir = home ++ "/.stack/programs"
  nextDirs <- listDirectory stackDir
  results <- forM nextDirs $ \subPath -> do
    let fullPath = stackDir ++ "/" ++ subPath
    subContents <- listDirectory fullPath
    return $ fmap ((++) (fullPath ++ "/")) (find (== ghcVersion) subContents)
  case catMaybes results of
    [] -> return Nothing
    (fp : _) -> return $ Just (fp ++ "/bin/ghc")

-- TODO: This doesn't necessarily account for having multiple snapshots that
--       both use 8.8.4. This logic definitely needs to be tighter.
findStackPackageDb :: IO (Maybe FilePath)
findStackPackageDb = do
  home <- getHomeDirectory
  let stackDir = home ++ "/.stack/snapshots"
  nextDirs <- listDirectory stackDir
  results <- forM nextDirs $ \subPath -> do
    -- 'fullPath' is like .stack/snapshots/linux_x86_64
    let fullPath = stackDir ++ "/" ++ subPath
    subContents <- listDirectory fullPath
    results' <- forM subContents $ \hashDirectory -> do
      -- 'fullHashPath' is like .stack/snapshots/linux_x86_64/77asdfasdf...
      let fullHashPath = fullPath ++ "/" ++ hashDirectory
      allGhcDirs <- listDirectory fullHashPath
      return $ fmap ((++) (fullHashPath ++ "/")) (find (== ghcVersionNumber) allGhcDirs)
    return $ catMaybes results'
  case concat results of
    [] -> return Nothing
    (pkgPath : _) -> return (Just $ pkgPath ++ "/pkgdb/")

-- BFS
-- Assumes home is NOT the project
findProjectRoot :: IO (Maybe FilePath)
findProjectRoot = do
  home <- getHomeDirectory
  findProjectRootTail (S.singleton home)
  where
    findProjectRootTail :: S.Seq FilePath -> IO (Maybe FilePath)
    findProjectRootTail queue = case S.viewl queue of
      S.EmptyL -> return Nothing
      (currentRoot S.:< rest) -> do
        if isSuffixOf ('/' : projectRootDirName) currentRoot
          then return (Just currentRoot)
          else do
            contents <- listDirectory currentRoot
            results <- forM contents $ \c -> do
              let newDir = currentRoot ++ "/" ++ c
              isDir <- doesDirectoryExist newDir
              if isDir
                then return (Just newDir)
                else return Nothing
            findProjectRootTail $ rest S.>< S.fromList (catMaybes results)

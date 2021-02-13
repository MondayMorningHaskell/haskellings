module DirectoryUtils where

import Control.Exception (catch, Exception)
import Data.List (isInfixOf, isSuffixOf)
import Data.List.Extra (takeWhileEnd)
import qualified Data.Sequence as S
import System.Directory
import System.Info (os)

basename :: FilePath -> FilePath
basename = takeWhileEnd (\c -> c /= '/' && c /= '\\')

-- Given an absolute file path and a relative file path, append them together,
-- correct for the OS.
pathJoin :: FilePath -> FilePath -> FilePath
pathJoin root path = root ++ makeRelative path

makeRelative :: FilePath -> FilePath
makeRelative path = if isWindows
  then "\\" ++ path
  else '/' : path

isWindows :: Bool
isWindows = os `notElem` ["linux", "macos", "unix"]

-- Using BFS, find the first file
searchForFileContaining :: FilePath -> String -> IO (Maybe FilePath)
searchForFileContaining searchRoot fileToFind = undefined
  where
    predicate fp = do
      fileExists <- doesFileExist fp
      return $ fileExists && fileToFind `isInfixOf` basename fp

searchForDirectoryContaining :: FilePath -> String -> IO (Maybe FilePath)
searchForDirectoryContaining searchRoot directoryToFind = fpBFS predicate (S.singleton searchRoot)
  where
    predicate fp = do
      isDirectory <- doesDirectoryExist fp
      return $ isDirectory && makeRelative directoryToFind `isSuffixOf` fp

-- TODO: Use this as the predicate in the appropriate place
-- makeRelative projectRootDirName `isSuffixOf` currentRoot

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
            let results = map (pathJoin currentRoot) contents
            fpBFS predicate $ rest S.>< S.fromList results
          else fpBFS predicate rest

-- Like 'listDirectory', but if an exception is encountered,
-- simply returns an empty list.
safeListDirectory :: FilePath -> IO [FilePath]
safeListDirectory fp = catch (listDirectory fp) emptyListHandler
  where
    emptyListHandler :: IOError -> IO [FilePath]
    emptyListHandler = const (return [])

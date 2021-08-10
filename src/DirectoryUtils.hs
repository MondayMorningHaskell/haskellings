module DirectoryUtils where

import           Control.Exception (Exception, catch)
import           Data.List         (dropWhileEnd, isInfixOf, isSuffixOf)
import           Data.List.Extra   (takeWhileEnd)
import qualified Data.Sequence     as S
import           System.Directory
import           System.FilePath   ((</>), takeFileName, takeDirectory)
import           System.Info       (os)

isWindows :: Bool
isWindows = os `notElem` ["linux", "unix", "darwin"]

returnIfDirExists :: FilePath -> IO (Maybe FilePath)
returnIfDirExists fp = do
  exists <- doesDirectoryExist fp
  if exists
    then return (Just fp)
    else return Nothing

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
            let results = map ((</>) currentRoot) contents
            fpBFS predicate $ rest S.>< S.fromList results
          else fpBFS predicate rest

-- Like 'listDirectory', but returns an empty list if there's a
-- permission error.
safeListDirectory :: FilePath -> IO [FilePath]
safeListDirectory fp = catch (listDirectory fp) emptyListHandler
  where
    emptyListHandler :: IOError -> IO [FilePath]
    emptyListHandler = const (return [])

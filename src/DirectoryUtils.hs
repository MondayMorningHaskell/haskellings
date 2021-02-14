module DirectoryUtils where

import Control.Exception (catch, Exception)
import Data.List (isInfixOf, isSuffixOf)
import Data.List.Extra (takeWhileEnd)
import qualified Data.Sequence as S
import System.Directory
import System.Info (os)

isWindows :: Bool
isWindows = os `notElem` ["linux", "macos", "unix"]

basename :: FilePath -> FilePath
basename = takeWhileEnd (\c -> c /= '/' && c /= '\\')

makeRelative :: FilePath -> FilePath
makeRelative path = if isRelativeBegin path
  then path
  else if isWindows
    then "\\" ++ path
    else '/' : path

isRelativeEnd :: FilePath -> Bool
isRelativeEnd "" = False
isRelativeEnd fp = last fp == '/' || last fp == '\\'

isRelativeBegin :: FilePath -> Bool
isRelativeBegin "" = False
isRelativeBegin fp = head fp == '/' || head fp == '\\'

pathJoin :: FilePath -> FilePath -> FilePath
pathJoin fp1 fp2 = case (isRelativeEnd fp1, isRelativeBegin fp2) of
  (True, True) -> fp1 ++ tail fp2
  (False, False) -> fp1 ++ makeRelative fp2
  _ -> fp1 ++ fp2

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
      return $ isDirectory && makeRelative directoryToFind `isSuffixOf` fp

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
            let results = map (pathJoin currentRoot) contents
            fpBFS predicate $ rest S.>< S.fromList results
          else fpBFS predicate rest

-- Like 'listDirectory', but returns an empty list if there's a
-- permission error.
safeListDirectory :: FilePath -> IO [FilePath]
safeListDirectory fp = catch (listDirectory fp) emptyListHandler
  where
    emptyListHandler :: IOError -> IO [FilePath]
    emptyListHandler = const (return [])

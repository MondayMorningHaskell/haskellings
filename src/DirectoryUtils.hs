module DirectoryUtils where

import           Control.Exception (Exception, catch)
import           Data.List         (dropWhileEnd, isInfixOf, isSuffixOf)
import           Data.List.Extra   (takeWhileEnd)
import qualified Data.Sequence     as S
import           System.Directory
import           System.Info       (os)

isWindows :: Bool
isWindows = os `notElem` ["linux", "unix", "darwin"]

basename :: FilePath -> FilePath
basename fp = takeWhileEnd (\c -> c /= '/' && c /= '\\') trimmedFp
  where
    trimmedFp = if isRelativeEnd fp then init fp else fp

-- Like doing "cd .." with this filepath
dropDirectoryLevel :: FilePath -> FilePath
dropDirectoryLevel fp = init $ dropWhileEnd (\c -> c /= '/' && c /= '\\') trimmedFp
  where
    trimmedFp = if isRelativeEnd fp then init fp else fp

makeRelative :: FilePath -> FilePath
makeRelative path = if isWindows
  then makeRelativeWindows path
  else makeRelativeUnix path

makeRelativeWindows :: FilePath -> FilePath
makeRelativeWindows = makeRelative' '\\'

makeRelativeUnix :: FilePath -> FilePath
makeRelativeUnix = makeRelative' '/'

makeRelative' :: Char -> FilePath -> FilePath
makeRelative' delimiter fp = if head fp == delimiter
  then fp
  else delimiter : fp

isRelativeEnd :: FilePath -> Bool
isRelativeEnd "" = False
isRelativeEnd fp = last fp == '/' || last fp == '\\'

isRelativeBegin :: FilePath -> Bool
isRelativeBegin "" = False
isRelativeBegin fp = head fp == '/' || head fp == '\\'

pathJoin :: FilePath -> FilePath -> FilePath
pathJoin fp1 fp2 = if isWindows
  then pathJoinWindows fp1 fp2
  else pathJoinUnix fp1 fp2

pathJoinUnix :: FilePath -> FilePath -> FilePath
pathJoinUnix = pathJoin' '/'

pathJoinWindows :: FilePath -> FilePath -> FilePath
pathJoinWindows = pathJoin' '\\'

pathJoin' :: Char -> FilePath -> FilePath -> FilePath
pathJoin' delimiter fp1 fp2 = case (isRelativeEnd fp1, isRelativeBegin fp2) of
  (True, True)   -> fp1 ++ tail fp2
  (False, False) -> fp1 ++ makeRelative' delimiter fp2
  _              -> fp1 ++ fp2

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

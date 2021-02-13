module DirectoryUtils where

import System.Info (os)
import Data.List.Extra (takeWhileEnd)

isWindows :: Bool
isWindows = os `notElem` ["linux", "macos", "unix"]

basename :: FilePath -> FilePath
basename = takeWhileEnd (\c -> c /= '/' && c /= '\\')

makeRelative :: FilePath -> FilePath
makeRelative path = if isWindows
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

module DirectoryUtils where

import System.Info (os)

isWindows :: Bool
isWindows = os `notElem` ["linux", "macos", "unix"]

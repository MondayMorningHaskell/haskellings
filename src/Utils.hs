module Utils where

import Control.Monad (void)
import Data.List
import Data.List.Extra
import System.Directory
import System.Process

fpBasename :: FilePath -> FilePath
fpBasename = takeWhileEnd (/= '/')

isHaskellFile :: FilePath -> Bool
isHaskellFile = isSuffixOf ".hs"

-- Probably a good idea to first check that it is a Haskell file first
haskellModuleName :: FilePath -> FilePath
haskellModuleName fp = dropEnd 3 (fpBasename fp)

compileExercise :: (FilePath, FilePath) -> (FilePath, FilePath) -> IO ()
compileExercise (projectRoot, ghcPath) (exDirectory, exFilename) = do
  let fullSourcePath = projectRoot ++ "/src/exercises/" ++ exDirectory ++ "/" ++ exFilename
  let genDirPath = projectRoot ++ "/generated_files/" ++ exDirectory
  let genExecutablePath = genDirPath ++ "/" ++ (haskellModuleName exFilename)
  createDirectoryIfMissing True genDirPath
  (_, _, _, procHandle) <- createProcess $
    proc ghcPath [fullSourcePath, "-odir", genDirPath, "-hidir", genDirPath, "-o", genExecutablePath]
  void $ waitForProcess procHandle

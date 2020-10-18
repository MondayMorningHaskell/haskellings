module Utils where

import Control.Monad (void, forM_)
import Data.Char
import Data.List
import Data.List.Extra
import System.Console.ANSI
import System.Directory
import System.Exit
import System.IO
import System.Process

import ExerciseList

fpBasename :: FilePath -> FilePath
fpBasename = takeWhileEnd (/= '/')

isHaskellFile :: FilePath -> Bool
isHaskellFile = isSuffixOf ".hs"

-- Probably a good idea to first check that it is a Haskell file first
haskellModuleName :: FilePath -> FilePath
haskellModuleName fp = dropEnd 3 (fpBasename fp)

compileExercise :: (FilePath, FilePath) -> ExerciseInfo -> IO ExitCode
compileExercise (projectRoot, ghcPath) (_, exDirectory, exFilename) = do
  let fullSourcePath = projectRoot ++ "/src/exercises/" ++ exDirectory ++ "/" ++ exFilename
  let genDirPath = projectRoot ++ "/generated_files/" ++ exDirectory
  let genExecutablePath = genDirPath ++ "/" ++ (haskellModuleName exFilename)
  createDirectoryIfMissing True genDirPath
  let processSpec = proc ghcPath [fullSourcePath, "-odir", genDirPath, "-hidir", genDirPath, "-o", genExecutablePath]
  (_, _, procStdErr, procHandle) <- createProcess (processSpec { std_out = CreatePipe, std_err = CreatePipe })
  exitCode <- waitForProcess procHandle
  case exitCode of
    ExitSuccess -> do
      setSGR [SetColor Foreground Vivid Green]
      putStrLn $ "Successfully compiled : " ++ exFilename
      setSGR [Reset]
      removeDirectoryRecursive genDirPath
      return exitCode
    ExitFailure code -> do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "Couldn't compile : " ++ exFilename
      case procStdErr of
        Nothing -> return ()
        Just h -> hGetContents h >>= putStrLn
      setSGR [Reset]
      removeDirectoryRecursive genDirPath
      return exitCode

compileExercise_ :: (FilePath, FilePath) -> ExerciseInfo -> IO ()
compileExercise_ ps ex = void $ compileExercise ps ex

fileContainsNotDone :: FilePath -> IO Bool
fileContainsNotDone fullFp = do
  fileLines <- lines <$> readFile fullFp
  return (any isDoneLine fileLines)
  where
    isDoneLine :: String -> Bool
    isDoneLine l = (upper . (filter (not . isSpace)) $ l) == "--IAMNOTDONE"

fullExerciseFp :: FilePath -> ExerciseInfo -> FilePath
fullExerciseFp projectRoot (_, exDir, exFile) = projectRoot ++ "/src/exercises/" ++ exDir ++ "/" ++ exFile

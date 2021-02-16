module Utils where

import           Control.Monad       (forM_, void)
import           Data.Char
import           Data.List
import           Data.List.Extra
import           System.Console.ANSI
import           System.Directory
import           System.Exit
import           System.IO
import           System.Process

import           Config
import           DirectoryUtils
import           ExerciseList

isHaskellFile :: FilePath -> Bool
isHaskellFile = isSuffixOf ".hs"

-- Probably a good idea to first check that it is a Haskell file first
haskellModuleName :: FilePath -> FilePath
haskellModuleName fp = dropEnd 3 (basename fp)

haskellFileName :: FilePath -> FilePath
haskellFileName exName = exName ++ ".hs"

data RunResult =
  CompileError | TestFailed | RunSuccess
  deriving (Show, Eq)

compileExercise :: ProgramConfig -> ExerciseInfo -> IO RunResult
compileExercise config (ExerciseInfo exerciseName exDirectory exIsRunnable _) = do
  let exFilename = haskellFileName exerciseName
  let root = projectRoot config
  let fullSourcePath = root `pathJoin` exercisesExt config `pathJoin` exDirectory `pathJoin` exFilename
  let genDirPath = root `pathJoin` "/generated_files/" `pathJoin` exDirectory
  let genExecutablePath = genDirPath `pathJoin` haskellModuleName exFilename
  createDirectoryIfMissing True genDirPath
  let baseArgs = [fullSourcePath, "-odir", genDirPath, "-hidir", genDirPath]
  let execArgs = if exIsRunnable then baseArgs ++ ["-o", genExecutablePath] else baseArgs
  let finalArgs = case packageDb config of
        Nothing      -> execArgs
        Just pkgPath -> execArgs ++ ["-package-db", pkgPath]
  let processSpec = proc (ghcPath config) finalArgs
  (_, _, procStdErr, procHandle) <- createProcess (processSpec { std_out = CreatePipe, std_err = CreatePipe })
  exitCode <- waitForProcess procHandle
  case exitCode of
    ExitFailure code -> do
      setSGR [SetColor Foreground Vivid Red]
      progPutStrLn config $ "Couldn't compile : " ++ exFilename
      case procStdErr of
        Nothing -> return ()
        Just h  -> hGetContents h >>= progPutStrLn config
      setSGR [Reset]
      removeDirectoryRecursive genDirPath
      return CompileError
    ExitSuccess -> do
      setSGR [SetColor Foreground Vivid Green]
      progPutStrLn config $ "Successfully compiled : " ++ exFilename
      setSGR [Reset]
      if not exIsRunnable
        then removeDirectoryRecursive genDirPath >> return RunSuccess
        else do
          let execSpec = shell genExecutablePath
          (_, _, execStdErr, execProcHandle) <- createProcess (execSpec { std_out = CreatePipe, std_err = CreatePipe })
          execExit <- waitForProcess execProcHandle
          case execExit of
            ExitFailure code -> do
              setSGR [SetColor Foreground Vivid Red]
              progPutStrLn config $ "Tests failed on exercise : " ++ exFilename
              case execStdErr of
                Nothing -> return ()
                Just h  -> hGetContents h >>= progPutStrLn config
              setSGR [Reset]
              removeDirectoryRecursive genDirPath
              return TestFailed
            ExitSuccess -> do
              setSGR [SetColor Foreground Vivid Green]
              progPutStrLn config $ "Successfully ran : " ++ exFilename
              setSGR [Reset]
              removeDirectoryRecursive genDirPath
              return RunSuccess

compileExercise_ :: ProgramConfig -> ExerciseInfo -> IO ()
compileExercise_ config ex = void $ compileExercise config ex

fileContainsNotDone :: FilePath -> IO Bool
fileContainsNotDone fullFp = do
  fileLines <- lines <$> readFile fullFp
  return (any isDoneLine fileLines)
  where
    isDoneLine :: String -> Bool
    isDoneLine l = (upper . filter (not . isSpace) $ l) == "--IAMNOTDONE"

fullExerciseFp :: FilePath -> FilePath -> ExerciseInfo -> FilePath
fullExerciseFp projectRoot exercisesExt (ExerciseInfo exName exDir _ _) = projectRoot `pathJoin` exercisesExt `pathJoin` exDir `pathJoin` haskellFileName exName

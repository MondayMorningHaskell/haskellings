module Utils where

import           Control.Monad    (forM_, void, when)
import           Data.Char
import           Data.List
import           Data.List.Extra
import           Data.Maybe       (fromJust, isJust)
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

executeExercise :: ProgramConfig -> ExerciseInfo -> IO ()
executeExercise config exInfo@(ExerciseInfo exerciseName _ _ _) = do
  let (processSpec, genDirPath, genExecutablePath) = createExerciseProcess config exInfo
  let exFilename = haskellFileName exerciseName
  withDirectory genDirPath $ do
    (_, _, procStdErr, procHandle) <- createProcess (processSpec { std_out = CreatePipe, std_err = CreatePipe })
    exitCode <- waitForProcess procHandle
    case exitCode of
      ExitFailure code -> void $ onCompileFailure config exFilename procStdErr
      ExitSuccess -> do
        progPutStrLnSuccess config $ "Successfully compiled: " ++ exFilename
        progPutStrLn config $ "----- Executing file: " ++ exFilename ++ " -----"
        let execSpec = shell genExecutablePath
        (_, _, _, execProcHandle) <- createProcess execSpec
        void $ waitForProcess execProcHandle

-- Produces 3 Elements for running our exercise:
-- 1. The 'CreateProcess' that we can run for the compilation.
-- 2. The directory path for the generated files
-- 3. The path of the executable we would run (assuming the exercise is executable).
createExerciseProcess :: ProgramConfig -> ExerciseInfo -> (CreateProcess, FilePath, FilePath)
createExerciseProcess config (ExerciseInfo exerciseName exDirectory exType _) =
  (processSpec, genDirPath, genExecutablePath)
  where
    exIsRunnable = exType /= CompileOnly
    exFilename = haskellFileName exerciseName
    root = projectRoot config
    fullSourcePath = root `pathJoin` exercisesExt config `pathJoin` exDirectory `pathJoin` exFilename
    genDirPath = root `pathJoin` "/generated_files/" `pathJoin` exDirectory
    genExecutablePath = genDirPath `pathJoin` haskellModuleName exFilename
    baseArgs = [fullSourcePath, "-odir", genDirPath, "-hidir", genDirPath]
    execArgs = if exIsRunnable then baseArgs ++ ["-o", genExecutablePath] else baseArgs
    finalArgs = execArgs ++ ["-package-db", packageDb config]
    processSpec = proc (ghcPath config) finalArgs

onCompileFailure :: ProgramConfig -> String -> Maybe Handle -> IO RunResult
onCompileFailure config exFilename errHandle = withTerminalFailure $ do
  progPutStrLn config $ "Couldn't compile : " ++ exFilename
  case errHandle of
    Nothing -> return ()
    Just h  -> hGetContents h >>= progPutStrLn config
  return CompileError

runUnitTestExercise :: ProgramConfig -> FilePath -> String -> IO RunResult
runUnitTestExercise config genExecutablePath exFilename = do
  let execSpec = shell genExecutablePath
  (_, execStdOut, execStdErr, execProcHandle) <- createProcess (execSpec { std_out = CreatePipe, std_err = CreatePipe })
  execExit <- waitForProcess execProcHandle
  case execExit of
    ExitFailure code -> withTerminalFailure $ do
      progPutStrLn config $ "Tests failed on exercise : " ++ exFilename
      case execStdErr of
        Nothing -> return ()
        Just h  -> hGetContents h >>= progPutStrLn config
      case execStdOut of
        Nothing -> return ()
        Just h  -> hGetContents h >>= progPutStrLn config
      return TestFailed
    ExitSuccess -> do
      progPutStrLnSuccess config $ "Successfully ran : " ++ exFilename
      return RunSuccess

runExecutableExercise
  :: ProgramConfig
  -> FilePath
  -> String
  -> [String]
  -> ([String] -> Bool)
  -> IO RunResult
runExecutableExercise config genExecutablePath exFilename inputs outputPred = do
  let execSpec = shell genExecutablePath
  (execStdIn, execStdOut, execStdErr, execProcHandle) <- createProcess
    (execSpec { std_out = CreatePipe, std_err = CreatePipe, std_in = CreatePipe })
  when (isJust execStdIn) $ forM_ inputs $ \i -> hPutStrLn (fromJust execStdIn) i
  execExit <- waitForProcess execProcHandle
  case execExit of
    ExitFailure code -> withTerminalFailure $ do
      progPutStrLn config $ "Encountered error running exercise: " ++ exFilename
      case execStdOut of
        Nothing -> return ()
        Just h  -> hGetContents h >>= progPutStrLn config
      case execStdErr of
        Nothing -> return ()
        Just h  -> hGetContents h >>= progPutStrLn config
      progPutStrLn config "Check the Sample Input and Sample Output in the file."
      progPutStrLn config $ "Then try running it for yourself with 'haskellings exec" ++ haskellModuleName exFilename ++ "'."
      return TestFailed
    ExitSuccess -> do
      passes <- case execStdOut of
        Nothing -> return (outputPred [])
        Just h  -> (lines <$> hGetContents h) >>= (return . outputPred)
      if passes
        then withTerminalSuccess $ do
          progPutStrLn config $ "Successfully ran : " ++ exFilename
          progPutStrLn config $ "You can run this code for yourself with 'haskellings exec " ++ haskellModuleName exFilename ++ "'."
          return RunSuccess
        else withTerminalFailure $ do
          progPutStrLn config $ "Unexpected output for exercise: " ++ exFilename
          progPutStrLn config "Check the Sample Input and Sample Output in the file."
          progPutStrLn config $ "Then try running it for yourself with 'haskellings exec " ++ haskellModuleName exFilename ++ "'."
          return TestFailed

compileAndRunExercise :: ProgramConfig -> ExerciseInfo -> IO RunResult
compileAndRunExercise config exInfo@(ExerciseInfo exerciseName exDirectory exType _) = do
  let (processSpec, genDirPath, genExecutablePath) = createExerciseProcess config exInfo
  let exFilename = haskellFileName exerciseName
  withDirectory genDirPath $ do
    (_, _, procStdErr, procHandle) <- createProcess (processSpec { std_out = CreatePipe, std_err = CreatePipe })
    exitCode <- waitForProcess procHandle
    case exitCode of
      ExitFailure code -> onCompileFailure config exFilename procStdErr
      ExitSuccess -> do
        progPutStrLnSuccess config $ "Successfully compiled : " ++ exFilename
        case exType of
          CompileOnly -> return RunSuccess
          UnitTests -> runUnitTestExercise config genExecutablePath exFilename
          Executable inputs outputPred -> runExecutableExercise config genExecutablePath exFilename inputs outputPred

compileAndRunExercise_ :: ProgramConfig -> ExerciseInfo -> IO ()
compileAndRunExercise_ config ex = void $ compileAndRunExercise config ex

fileContainsNotDone :: FilePath -> IO Bool
fileContainsNotDone fullFp = do
  fileLines <- lines <$> readFile fullFp
  return (any isDoneLine fileLines)
  where
    isDoneLine :: String -> Bool
    isDoneLine l = (upper . filter (not . isSpace) $ l) == "--IAMNOTDONE"

fullExerciseFp :: FilePath -> FilePath -> ExerciseInfo -> FilePath
fullExerciseFp projectRoot exercisesExt (ExerciseInfo exName exDir _ _) = projectRoot `pathJoin` exercisesExt `pathJoin` exDir `pathJoin` haskellFileName exName

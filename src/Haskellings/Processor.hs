{-|
Module      : Haskellings.Processor
Description : Functions for compiling and executing exercises.
License     : BSD3
Maintainer  : james@mondaymorninghaskell.me

These functions compile exercise files and, if applicable,
run the generated executables to unit test them. The implementation
details are largely centered around constructing GHC commands,
running those through System.Process and analyzing the results.
-}

module Haskellings.Processor (
  -- * Compiling exercises and running tests
  compileAndRunExercise,
  compileAndRunExercise_,
  -- * Executing an exercise for custom input
  executeExercise
) where

import           Control.Monad.Reader
import           Data.Maybe                 (fromJust, isJust)
import           System.Exit
import           System.FilePath            ((</>))
import           System.IO
import           System.Process

import           Haskellings.DirectoryUtils
import           Haskellings.TerminalUtils
import           Haskellings.Types

-- | Compiles the given exercise and, if applicable, runs the unit tests
--   or executable tests associated with it.
compileAndRunExercise :: ExerciseInfo -> ReaderT ProgramConfig IO RunResult
compileAndRunExercise exInfo@(ExerciseInfo exerciseName exDirectory exType _) = do
  config <- ask
  let (processSpec, genDirPath, genExecutablePath, exFilename) = createExerciseProcess config exInfo
  withDirectory genDirPath $ do
    (_, _, procStdErr, procHandle) <- lift $ createProcess (processSpec { std_out = CreatePipe, std_err = CreatePipe })
    exitCode <- lift $ waitForProcess procHandle
    case exitCode of
      ExitFailure code -> onCompileFailure exFilename procStdErr
      ExitSuccess -> do
        progPutStrLnSuccess $ "Successfully compiled : " ++ exFilename
        case exType of
          CompileOnly -> return RunSuccess
          UnitTests -> runUnitTestExercise genExecutablePath exFilename
          Executable inputs outputPred -> runExecutableExercise genExecutablePath exFilename inputs outputPred

-- | Same as 'compileAndRunExercise', but discards the 'RunResult'.
compileAndRunExercise_ :: ExerciseInfo -> ReaderT ProgramConfig IO ()
compileAndRunExercise_ ex = void $ compileAndRunExercise ex

-- | 'Execute' an exercise, allowing the user to run the program
--   with their own input and examine the output. This only really works
--   for "Executable" exercises.
executeExercise :: ExerciseInfo -> ReaderT ProgramConfig IO ()
executeExercise exInfo@(ExerciseInfo exerciseName _ _ _) = do
  config <- ask
  let (processSpec, genDirPath, genExecutablePath, exFilename) = createExerciseProcess config exInfo
  withDirectory genDirPath $ do
    (_, _, procStdErr, procHandle) <- lift $ createProcess (processSpec { std_out = CreatePipe, std_err = CreatePipe })
    exitCode <- lift $ waitForProcess procHandle
    case exitCode of
      ExitFailure code -> void $ onCompileFailure exFilename procStdErr
      ExitSuccess -> do
        progPutStrLnSuccess $ "Successfully compiled: " ++ exFilename
        progPutStrLn $ "----- Executing file: " ++ exFilename ++ " -----"
        let execSpec = shell genExecutablePath
        (_, _, _, execProcHandle) <- lift $ createProcess execSpec
        void $ lift $ waitForProcess execProcHandle

---------- PRIVATE FUNCTIONS ----------

-- Produces 3 Elements for running our exercise:
-- 1. The 'CreateProcess' that we can run for the compilation.
-- 2. The directory path for the generated files
-- 3. The path of the executable we would run (assuming the exercise is executable).
createExerciseProcess :: ProgramConfig -> ExerciseInfo -> (CreateProcess, FilePath, FilePath, FilePath)
createExerciseProcess config (ExerciseInfo exerciseName exDirectory exType _) =
  (processSpec, genDirPath, genExecutablePath, haskellFileName exerciseName)
  where
    exIsRunnable = exType /= CompileOnly
    exFilename = haskellFileName exerciseName
    root = projectRoot config
    fullSourcePath = root </> exercisesExt config </> exDirectory </> exFilename
    genDirPath = root </> "generated_files" </> exDirectory
    genExecutablePath = genDirPath </> haskellModuleName exFilename
    baseArgs = [fullSourcePath, "-odir", genDirPath, "-hidir", genDirPath]
    execArgs = if exIsRunnable then baseArgs ++ ["-o", genExecutablePath] else baseArgs
    finalArgs = execArgs ++ ["-package-db", packageDb config]
    processSpec = proc (ghcPath config) finalArgs

onCompileFailure :: String -> Maybe Handle -> ReaderT ProgramConfig IO RunResult
onCompileFailure exFilename errHandle = withTerminalFailure $ do
  progPutStrLn $ "Couldn't compile : " ++ exFilename
  case errHandle of
    Nothing -> return ()
    Just h  -> lift (hGetContents h) >>= progPutStrLn
  return CompileError

runUnitTestExercise :: FilePath -> String -> ReaderT ProgramConfig IO RunResult
runUnitTestExercise genExecutablePath exFilename = do
  let execSpec = shell genExecutablePath
  (_, execStdOut, execStdErr, execProcHandle) <- lift $ createProcess (execSpec { std_out = CreatePipe, std_err = CreatePipe })
  execExit <- lift $ waitForProcess execProcHandle
  case execExit of
    ExitFailure code -> withTerminalFailure $ do
      progPutStrLn $ "Tests failed on exercise : " ++ exFilename
      case execStdErr of
        Nothing -> return ()
        Just h  -> lift (hGetContents h) >>= progPutStrLn
      case execStdOut of
        Nothing -> return ()
        Just h  -> lift (hGetContents h) >>= progPutStrLn
      return TestFailed
    ExitSuccess -> do
      progPutStrLnSuccess $ "Successfully ran : " ++ exFilename
      return RunSuccess

runExecutableExercise
  :: FilePath
  -> String
  -> [String]
  -> ([String] -> Bool)
  -> ReaderT ProgramConfig IO RunResult
runExecutableExercise genExecutablePath exFilename inputs outputPred = do
  let execSpec = shell genExecutablePath
  (execStdIn, execStdOut, execStdErr, execProcHandle) <- lift $ createProcess
    (execSpec { std_out = CreatePipe, std_err = CreatePipe, std_in = CreatePipe })
  when (isJust execStdIn) $ forM_ inputs $ \i -> lift $ hPutStrLn (fromJust execStdIn) i
  execExit <- lift $ waitForProcess execProcHandle
  case execExit of
    ExitFailure code -> withTerminalFailure $ do
      progPutStrLn $ "Encountered error running exercise: " ++ exFilename
      case execStdOut of
        Nothing -> return ()
        Just h  -> lift (hGetContents h) >>= progPutStrLn
      case execStdErr of
        Nothing -> return ()
        Just h  -> lift (hGetContents h) >>= progPutStrLn
      progPutStrLn "Check the Sample Input and Sample Output in the file."
      progPutStrLn $ "Then try running it for yourself with 'haskellings exec" ++ haskellModuleName exFilename ++ "'."
      return TestFailed
    ExitSuccess -> do
      passes <- case execStdOut of
        Nothing -> return (outputPred [])
        Just h  -> (lines <$> lift (hGetContents h)) >>= (return . outputPred)
      if passes
        then withTerminalSuccess $ do
          progPutStrLn $ "Successfully ran : " ++ exFilename
          progPutStrLn $ "You can run this code for yourself with 'haskellings exec " ++ haskellModuleName exFilename ++ "'."
          return RunSuccess
        else withTerminalFailure $ do
          progPutStrLn $ "Unexpected output for exercise: " ++ exFilename
          progPutStrLn "Check the Sample Input and Sample Output in the file."
          progPutStrLn $ "Then try running it for yourself with 'haskellings exec " ++ haskellModuleName exFilename ++ "'."
          return TestFailed

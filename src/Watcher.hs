module Watcher where

import Control.Concurrent
import Control.Monad (forever, when, unless)
import System.Exit
import System.FSNotify
import qualified Data.Map as M

import ExerciseList
import Utils

watchExercises :: (FilePath, FilePath) -> IO ()
watchExercises paths = runExerciseWatch paths exerciseList

shouldCheckFile :: ExerciseInfo -> Event -> Bool
shouldCheckFile (_, _, exFile) (Modified fp _ _) = fpBasename fp == exFile
shouldCheckFile _ _ = False

-- This event should be a modification of one of our exercise files
processEvent :: (FilePath, FilePath) -> ExerciseInfo -> MVar () -> Event -> IO ()
processEvent configPaths exerciseInfo@(modName, exDirectory, exFile) signalMVar _ = do
  putStrLn $ "Running exercise: " ++ modName
  exitCode <- compileExercise configPaths exerciseInfo
  case exitCode of
    ExitSuccess -> do
      isNotDone <- fileContainsNotDone fullFp
      if isNotDone
        then putStrLn "This exercise compiles! Remove 'I AM NOT DONE' to proceed!"
        else putMVar signalMVar ()
    ExitFailure _ -> return ()
  where
    fullFp = fullExerciseFp (fst configPaths) exerciseInfo

runExerciseWatch :: (FilePath, FilePath) -> [ExerciseInfo] -> IO ()
runExerciseWatch _ [] = putStrLn "Congratulations, you've completed all the exercises!"
runExerciseWatch paths@(projectRoot, _) (firstEx : restExs) = do
  exitCode <- compileExercise paths firstEx
  isDone <- not <$> fileContainsNotDone fullFp
  if exitCode == ExitSuccess && isDone
    then runExerciseWatch paths restExs
    else do
      when (exitCode == ExitSuccess) $ putStrLn "This exercise compiles! Remove 'I AM NOT DONE' to proceed!"
      let conf = defaultConfig { confDebounce = Debounce 1 }
      withManagerConf conf $ \mgr -> do
        signalMVar <- newEmptyMVar
        stopAction <- watchTree mgr (projectRoot ++ "/src/exercises") (shouldCheckFile firstEx)
          (processEvent paths firstEx signalMVar)
        takeMVar signalMVar
        stopAction
      runExerciseWatch paths restExs
  where
    fullFp = fullExerciseFp projectRoot firstEx

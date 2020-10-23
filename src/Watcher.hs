module Watcher where

import Control.Concurrent
import Control.Monad (forever, when, unless)
import System.Exit
import System.FSNotify
import qualified Data.Map as M

import Config
import ExerciseList
import Utils

watchExercises :: ProgramConfig -> IO ()
watchExercises config = runExerciseWatch config exerciseList

shouldCheckFile :: ExerciseInfo -> Event -> Bool
shouldCheckFile (ExerciseInfo _ _ exFile) (Modified fp _ _) = fpBasename fp == exFile
shouldCheckFile _ _ = False

-- This event should be a modification of one of our exercise files
processEvent :: ProgramConfig -> ExerciseInfo -> MVar () -> Event -> IO ()
processEvent config exerciseInfo signalMVar _ = do
  progPutStrLn config $ "Running exercise: " ++ exerciseName exerciseInfo
  exitCode <- compileExercise config exerciseInfo
  case exitCode of
    ExitSuccess -> do
      isNotDone <- fileContainsNotDone fullFp
      if isNotDone
        then progPutStrLn config "This exercise compiles! Remove 'I AM NOT DONE' to proceed!"
        else putMVar signalMVar ()
    ExitFailure _ -> return ()
  where
    fullFp = fullExerciseFp (projectRoot config) (exercisesExt config) exerciseInfo

runExerciseWatch :: ProgramConfig -> [ExerciseInfo] -> IO ()
runExerciseWatch config [] = progPutStrLn config "Congratulations, you've completed all the exercises!"
runExerciseWatch config (firstEx : restExs) = do
  exitCode <- compileExercise config firstEx
  isDone <- not <$> fileContainsNotDone fullFp
  if exitCode == ExitSuccess && isDone
    then runExerciseWatch config restExs
    else do
      when (exitCode == ExitSuccess) $ progPutStrLn config "This exercise compiles! Remove 'I AM NOT DONE' to proceed!"
      let conf = defaultConfig { confDebounce = Debounce 1 }
      withManagerConf conf $ \mgr -> do
        signalMVar <- newEmptyMVar
        stopAction <- watchTree mgr ((projectRoot config) ++ (exercisesExt config)) (shouldCheckFile firstEx)
          (processEvent config firstEx signalMVar)
        takeMVar signalMVar
        stopAction
      runExerciseWatch config restExs
  where
    fullFp = fullExerciseFp (projectRoot config) (exercisesExt config) firstEx

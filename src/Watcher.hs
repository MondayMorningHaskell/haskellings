module Watcher where

import Control.Concurrent
import Control.Monad (forever, when, unless)
import System.Exit
import System.FSNotify
import System.IO (hIsEOF, hFlush, hClose)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M

import Config
import DirectoryUtils
import ExerciseList
import Utils

watchExercises :: ProgramConfig -> IO ()
watchExercises config = runExerciseWatch config exerciseList

shouldCheckFile :: ExerciseInfo -> Event -> Bool
shouldCheckFile (ExerciseInfo _ _ exFile _ _) (Modified fp _ _) = basename fp == exFile
shouldCheckFile (ExerciseInfo _ _ exFile _ _) (Added fp _ _) = basename fp == exFile
shouldCheckFile _ _ = False

-- This event should be a modification of one of our exercise files
processEvent :: ProgramConfig -> ExerciseInfo -> MVar () -> Event -> IO ()
processEvent config exerciseInfo signalMVar _ = do
  progPutStrLn config $ "Running exercise: " ++ exerciseName exerciseInfo
  runResult <- compileExercise config exerciseInfo
  case runResult of
    RunSuccess -> do
      isNotDone <- fileContainsNotDone fullFp
      if isNotDone
        then progPutStrLn config "This exercise succeeds! Remove 'I AM NOT DONE' to proceed!"
        else putMVar signalMVar ()
    _ -> return ()
  where
    fullFp = fullExerciseFp (projectRoot config) (exercisesExt config) exerciseInfo

runExerciseWatch :: ProgramConfig -> [ExerciseInfo] -> IO ()
runExerciseWatch config [] = progPutStrLn config "Congratulations, you've completed all the exercises!"
runExerciseWatch config (firstEx : restExs) = do
  threadDelay 1000000
  runResult <- compileExercise config firstEx
  isDone <- not <$> fileContainsNotDone fullFp
  if runResult == RunSuccess && isDone
    then runExerciseWatch config restExs
    else do
      when (runResult == RunSuccess) $ progPutStrLn config "This exercise succeeds! Remove 'I AM NOT DONE' to proceed!"
      let conf = defaultConfig { confDebounce = Debounce 1 }
      withManagerConf conf $ \mgr -> do
        signalMVar <- newEmptyMVar
        let dirToWatch = init (projectRoot config ++ exercisesExt config)
        stopAction <- watchTree mgr dirToWatch (shouldCheckFile firstEx)
          (processEvent config firstEx signalMVar)
        userInputThread <- forkIO $ forever (watchForUserInput config firstEx)
        takeMVar signalMVar
        stopAction
        -- TODO: killThread seems to block on Windows while waiting
        --       for hIsEOF. This doesn't seem like the best solution,
        --       since there doesn't seem to be a guarantee that the
        --       input thread will be killed for each exercise.
        forkIO $ killThread userInputThread
      runExerciseWatch config restExs
  where
    fullFp = fullExerciseFp (projectRoot config) (exercisesExt config) firstEx

watchForUserInput :: ProgramConfig -> ExerciseInfo -> IO ()
watchForUserInput config exInfo = do
  inIsEnd <- hIsEOF (inHandle config)
  if inIsEnd
    then threadDelay 1000000 >> return ()
    else do
      userInput <- progReadLine config
      when (userInput == "hint") $
        progPutStrLn config (exerciseHint exInfo)

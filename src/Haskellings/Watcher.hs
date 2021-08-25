{- Handles the "Watcher", which reruns exercises automatically
   whenever a file changes.
-}
module Haskellings.Watcher (
  watchExercises,
  runExerciseWatch
) where

import           Control.Concurrent
import           Control.Monad.Reader
import           System.FilePath      (takeFileName, (</>))
import           System.FSNotify
import           System.IO            (hIsEOF)

import           Haskellings.DirectoryUtils
import           Haskellings.Internal.ExerciseList
import           Haskellings.Processor
import           Haskellings.TerminalUtils
import           Haskellings.Types

watchExercises :: ReaderT ProgramConfig IO ()
watchExercises = runExerciseWatch allExercises

runExerciseWatch :: [ExerciseInfo] -> ReaderT ProgramConfig IO ()
runExerciseWatch [] = progPutStrLn "Congratulations, you've completed all the exercises!"
runExerciseWatch (firstEx : restExs) = do
  config <- ask
  let fullFp = fullExerciseFp (projectRoot config) (exercisesExt config) firstEx
  (runResult, isDone) <- withFileLock fullFp $ do
    runResult <- compileAndRunExercise firstEx
    isDone <- lift (not <$> fileContainsNotDone fullFp)
    return (runResult, isDone)
  if runResult == RunSuccess && isDone
    then runExerciseWatch restExs
    else do
      when (runResult == RunSuccess) $ progPutStrLn "This exercise succeeds! Remove 'I AM NOT DONE' to proceed!"
      let conf = defaultConfig { confDebounce = Debounce 1 }
      liftIO $ withManagerConf conf $ \mgr -> do
        signalMVar <- newEmptyMVar
        stopAction <- watchTree mgr (projectRoot config </> exercisesExt config) (shouldCheckFile firstEx)
          (\event -> runReaderT (processEvent firstEx signalMVar event) config)
        userInputThread <- forkIO $ forever (watchForUserInput config firstEx)
        takeMVar signalMVar
        stopAction
        forkIO $ killThread userInputThread
      runExerciseWatch restExs

---------- PRIVATE FUNCTIONS ----------
shouldCheckFile :: ExerciseInfo -> Event -> Bool
shouldCheckFile (ExerciseInfo exName _ _ _) (Added fp _ _) = takeFileName fp == haskellFileName exName
shouldCheckFile (ExerciseInfo exName _ _ _) (Modified fp _ _) = takeFileName fp == haskellFileName exName
shouldCheckFile _ _ = False

-- This event should be a modification of one of our exercise files
processEvent :: ExerciseInfo -> MVar () -> Event -> ReaderT ProgramConfig IO ()
processEvent exerciseInfo signalMVar _ = do
  config <- ask
  let fullFp = fullExerciseFp (projectRoot config) (exercisesExt config) exerciseInfo
  progPutStrLn $ "Running exercise: " ++ exerciseName exerciseInfo
  withFileLock fullFp $ do
    runResult <- compileAndRunExercise exerciseInfo
    case runResult of
      RunSuccess -> do
        isNotDone <- lift $ fileContainsNotDone fullFp
        if isNotDone
          then progPutStrLn "This exercise succeeds! Remove 'I AM NOT DONE' to proceed!"
          else lift $ putMVar signalMVar ()
      _ -> return ()

-- Must be IO because it is called through forkIO
watchForUserInput :: ProgramConfig -> ExerciseInfo -> IO ()
watchForUserInput config exInfo = do
  inIsEnd <- hIsEOF (inHandle config)
  if inIsEnd
    then void (threadDelay 1000000)
    else flip runReaderT config $ do
      userInput <- progReadLine
      when (userInput == "hint") $
        progPutStrLn (exerciseHint exInfo)

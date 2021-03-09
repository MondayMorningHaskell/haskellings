module RunCommands where

import           Control.Concurrent (threadDelay)
import           Control.Monad      (forM, forM_, mapM_, when)
import           Data.List          (maximumBy)
import qualified Data.Map           as M
import           System.Directory
import           System.Process

import           Config
import           ExerciseList
import           Utils

runExercise :: ProgramConfig -> String -> IO ()
runExercise config exerciseName = case M.lookup exerciseName allExercisesMap of
  Nothing -> progPutStrLn config $ "Could not find exercise: " ++ exerciseName ++ "!"
  Just exInfo -> compileAndRunExercise_ config exInfo

execExercise :: ProgramConfig -> String -> IO ()
execExercise config exerciseName = case M.lookup exerciseName allExercisesMap of
  Nothing -> progPutStrLn config $ "Could not find exercise: " ++ exerciseName ++ "!"
  Just exInfo@(ExerciseInfo _ _ (Executable _ _) _) -> executeExercise config exInfo
  _ -> progPutStrLn config $ "Exercise " ++ exerciseName ++ " is not executable!"

hintExercise :: ProgramConfig -> String -> IO ()
hintExercise config exerciseName = case M.lookup exerciseName allExercisesMap of
  Nothing -> progPutStrLn config $ "Could not find exercise: " ++ exerciseName ++ "!"
  Just exInfo -> progPutStrLn config (exerciseHint exInfo)

listExercises :: ProgramConfig -> IO ()
listExercises = listExercises' allExercises

-- Separated for testability
listExercises' :: [ExerciseInfo] -> ProgramConfig -> IO ()
listExercises' [] config = progPutStrLn config "No exercises!"
listExercises' exercises config = do
  progPutStrLn config "Listing exercises...(must remove \"I AM NOT DONE\" comment to indicate as done)"
  threadDelay 2000000
  let maxNameSize = maximum (length . exerciseName <$> exercises)
  forM_ (zip [1..] exercises) $ \(i, exInfo) -> do
    let fullFp = fullExerciseFp (projectRoot config) mainProjectExercisesDir exInfo
    let name = exerciseName exInfo
    isNotDone <- fileContainsNotDone fullFp
    let printNameAndDots = do
          when (i < 10) (progPutStr config " ")
          progPutStr config (show i)
          progPutStr config ": "
          progPutStr config name
          progPutStr config $ replicate (maxNameSize - length name) '.'
    if isNotDone
      then withTerminalFailure $ printNameAndDots >> putStrLn "...NOT DONE"
      else withTerminalSuccess $ printNameAndDots >> putStrLn ".......DONE"

runHelp :: IO ()
runHelp = mapM_ putStrLn
  [ "Available commands:"
  , "  haskellings watch             -- Run the Watcher for continuous exercise checking."
  , "  haskellings run {exercise}    -- Run an individual exercise."
  , "  haskellings exec {exercise}   -- Run an 'executable' exercise with custom input."
  , "  haskellings hint {exercise}   -- Display a hint for the exercise."
  , "  haskellings list              -- List all exercises and their status."
  , "  haskellings version           -- Display the current version of the program."
  , "  haskellings help (-h, --help) -- Display this help message."
  ]

module RunExercises where

import           Control.Monad    (forM)
import qualified Data.Map         as M
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

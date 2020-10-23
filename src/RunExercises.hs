module RunExercises where

import           Control.Monad (forM)
import qualified Data.Map as M
import           System.Directory
import           System.Process

import           Config
import           ExerciseList (exerciseMap)
import           Utils

runExercise :: ProgramConfig -> String -> IO ()
runExercise config exerciseName = case M.lookup exerciseName exerciseMap of
  Nothing -> progPutStrLn config $ "Could not find exercise: " ++ exerciseName ++ "!"
  Just exInfo -> compileExercise_ config exInfo

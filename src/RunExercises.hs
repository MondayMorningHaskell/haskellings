module RunExercises where

import           Control.Monad (forM)
import qualified Data.Map as M
import           System.Directory
import           System.Process

import           ExerciseList (exerciseMap)
import           Utils

runExercise :: (FilePath, FilePath) -> String -> IO ()
runExercise configPaths exerciseName = case M.lookup exerciseName exerciseMap of
  Nothing -> putStrLn $ "Could not find exercise: " ++ exerciseName ++ "!"
  Just exerciseInfo -> compileExercise_ configPaths exerciseInfo

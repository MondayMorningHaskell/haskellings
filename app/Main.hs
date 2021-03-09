module Main where

import           Data.Map           (empty)
import           System.Environment
import           System.IO

import           Config
import           RunExercises
import           Watcher

main :: IO ()
main = do
  args <- getArgs
  loadResult <- loadProjectRootAndGhc
  case loadResult of
    Left NoProjectRootError -> putStrLn "Couldn't find project root!"
    Left NoGhcError -> putStrLn $ "Couldn't find " ++ ghcVersion
    Right paths -> do
      packageDb <- findStackPackageDb
      let config = uncurry ProgramConfig paths packageDb mainProjectExercisesDir stdin stdout stderr empty
      if null args
        then progPutStrLn config "Haskellings requires a sub-command!"
        else do
          let command = head args
          case command of
            "run" -> if length args < 2
              then progPutStrLn config "Run command requires an exercise name!"
              else runExercise config (args !! 1)
            "watch" -> watchExercises config
            "exec" -> if length args < 2
              then progPutStrLn config "Exec command requires an exercise name!"
              else execExercise config (args !! 1)
            "version" -> putStrLn haskellingsVersion
            "list" -> listExercises config
            "hint" -> if length args < 2
              then progPutStrLn config "Hint command requires an exercise name!"
              else hintExercise config (args !! 1)
            _ -> progPutStrLn config $ command ++ " is not implemented yet!"

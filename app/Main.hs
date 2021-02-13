module Main where

import System.Environment
import System.IO

import Config
import RunExercises
import Watcher

main :: IO ()
main = do
  args <- getArgs
  loadResult <- loadProjectRootAndGhc
  case loadResult of
    Left NoProjectRootError -> putStrLn "Couldn't find project root!"
    Left NoGhcError -> putStrLn "Couldn't find ghc-8.8.4"
    Right paths -> do
      packageDb <- findStackPackageDb
      let config = ProgramConfig (fst paths) (snd paths) packageDb mainProjectExercisesDir stdin stdout stderr
      if null args
        then progPutStrLn config "Haskellings requires a sub-command!"
        else do
          let command = head args
          if command == "run" 
            then if length args < 2
              then progPutStrLn config "Run command requires an exercise name!"
              else runExercise config (args !! 1)
          else if command == "watch"
            then watchExercises config
            else progPutStrLn config $ command ++ " is not implemented yet!"

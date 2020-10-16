module Main where

import System.Environment

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
      if null args
        then putStrLn "Haskellings requires a sub-command!"
        else do
          let command = head args
          if command == "run" 
            then if length args < 2
              then putStrLn "Run command requires an exercise name!"
              else runExercise paths (args !! 1)
          else if command == "watch"
            then watchExercises paths
            else putStrLn $ command ++ " is not implemented yet!"

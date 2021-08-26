module Main where

import           Control.Monad.Reader
import           Data.Map                  (empty)
import           Data.Tuple.Extra          (uncurry3)
import           System.Environment
import           System.IO

import           Haskellings.Constants
import           Haskellings.LoadConfig
import           Haskellings.RunCommands
import           Haskellings.TerminalUtils
import           Haskellings.Types
import           Haskellings.Watcher

main :: IO ()
main = do
  args <- getArgs
  if null args || head args == "help" || head args == "-h" || head args == "--help"
    then runHelp
    else if head args == "configure"
      then runConfigure
      else do
        loadResult <- loadBaseConfigPaths
        case loadResult of
          Left NoProjectRootError -> putStrLn "Couldn't find project root!"
          Left NoGhcError -> putStrLn $ "Couldn't find " ++ ghcVersion
          Left NoStackPackageDbError -> putStrLn "Couldn't find an appropriate stack package DB!"
          Right paths -> do
            let config = uncurry3 ProgramConfig paths mainProjectExercisesDir stdin stdout stderr empty
            runReaderT (runCommand (tail args) (head args)) config

runCommand :: [String] -> String -> ReaderT ProgramConfig IO ()
runCommand restArgs command = case command of
  "run" -> if null restArgs
    then progPutStrLn "Run command requires an exercise name!"
    else runExercise (head restArgs)
  "watch" -> watchExercises
  "exec" -> if null restArgs
    then progPutStrLn "Exec command requires an exercise name!"
    else execExercise (head restArgs)
  "version" -> progPutStrLn haskellingsVersion
  "list" -> listExercises
  "hint" -> if null restArgs
    then progPutStrLn "Hint command requires an exercise name!"
    else hintExercise (head restArgs)
  _ -> lift runHelp

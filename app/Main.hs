module Main where

import           Data.Map           (empty)
import           Data.Tuple.Extra   (uncurry3)
import           System.Environment
import           System.IO

import           Config
import           RunCommands
import           Watcher

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
            runCommand config (tail args) (head args)

runCommand :: ProgramConfig -> [String] -> String -> IO ()
runCommand config restArgs command = case command of
  "run" -> if null restArgs
    then progPutStrLn config "Run command requires an exercise name!"
    else runExercise config (head restArgs)
  "watch" -> watchExercises config
  "exec" -> if null restArgs
    then progPutStrLn config "Exec command requires an exercise name!"
    else execExercise config (head restArgs)
  "version" -> putStrLn haskellingsVersion
  "list" -> listExercises config
  "hint" -> if null restArgs
    then progPutStrLn config "Hint command requires an exercise name!"
    else hintExercise config (head restArgs)
  _ -> runHelp

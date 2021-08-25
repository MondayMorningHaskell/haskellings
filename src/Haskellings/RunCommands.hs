{- Core wrapper functions for each commands,
   e.g. 'run', 'exec', 'hint', 'watch', 'list
-}
module Haskellings.RunCommands where

import           Control.Concurrent   (threadDelay)
import           Control.Monad.Reader
import qualified Data.Map             as M
import           Data.Yaml            (encodeFile)
import           System.Directory
import           System.FilePath      ((</>))

import           Haskellings.Constants
import           Haskellings.DirectoryUtils
import           Haskellings.Internal.ExerciseList
import           Haskellings.LoadConfig
import           Haskellings.Processor
import           Haskellings.TerminalUtils
import           Haskellings.Types

runExercise :: String -> ReaderT ProgramConfig IO ()
runExercise exerciseName = case M.lookup exerciseName allExercisesMap of
  Nothing -> progPutStrLn $ "Could not find exercise: " ++ exerciseName ++ "!"
  Just exInfo -> compileAndRunExercise_ exInfo

execExercise :: String -> ReaderT ProgramConfig IO ()
execExercise exerciseName = case M.lookup exerciseName allExercisesMap of
  Nothing -> progPutStrLn $ "Could not find exercise: " ++ exerciseName ++ "!"
  Just exInfo@(ExerciseInfo _ _ (Executable _ _) _) -> executeExercise exInfo
  _ -> progPutStrLn $ "Exercise " ++ exerciseName ++ " is not executable!"

hintExercise :: String -> ReaderT ProgramConfig IO ()
hintExercise exerciseName = case M.lookup exerciseName allExercisesMap of
  Nothing -> progPutStrLn $ "Could not find exercise: " ++ exerciseName ++ "!"
  Just exInfo -> progPutStrLn (exerciseHint exInfo)

listExercises :: ReaderT ProgramConfig IO ()
listExercises = listExercises' allExercises

-- Separated for testability
listExercises' :: [ExerciseInfo] -> ReaderT ProgramConfig IO ()
listExercises' [] = progPutStrLn "No exercises!"
listExercises' exercises = do
  config <- ask
  progPutStrLn "Listing exercises...(must remove \"I AM NOT DONE\" comment to indicate as done)"
  lift $ threadDelay 2000000
  let maxNameSize = maximum (length . exerciseName <$> exercises)
  forM_ (zip [1..] exercises) $ \(i, exInfo) -> do
    let fullFp = fullExerciseFp (projectRoot config) (exercisesExt config) exInfo
    let name = exerciseName exInfo
    isNotDone <- lift $ fileContainsNotDone fullFp
    let printNameAndDots = do
          when (i < 10) (progPutStr " ")
          progPutStr (show i)
          progPutStr ": "
          progPutStr name
          progPutStr $ replicate (maxNameSize - length name) '.'
    if isNotDone
      then withTerminalFailure $ printNameAndDots >> progPutStrLn "...NOT DONE"
      else withTerminalSuccess $ printNameAndDots >> progPutStrLn ".......DONE"

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

runConfigure :: IO ()
runConfigure = do
  projectRoot' <- findProjectRoot
  case projectRoot' of
    Nothing -> putStrLn "Could not find project root. Please move the repository so that it is somewhere within the 'home' directory on your system."
    Just projectRoot -> runConfigureWithProjectRoot projectRoot

runConfigureWithProjectRoot :: FilePath -> IO ()
runConfigureWithProjectRoot projectRoot = do
  putStrLn "Please enter GHC Path (or leave blank for default): "
  ghc <- getLine
  putStrLn "Please enter Stack package DB path (or leave blank): "
  stackPath <- getLine
  let configPath = projectRoot </> configFileName
  alreadyExists <- doesFileExist configPath
  when alreadyExists $ removeFile configPath
  let config = BaseConfig
                 (if null ghc then Nothing else Just ghc)
                 (if null stackPath then Nothing else Just stackPath)
  if null ghc && null stackPath
    then putStrLn "No configuration information given, will rely on defaults."
    else encodeFile configPath config >> putStrLn ("Saved configuration to " ++ configPath)

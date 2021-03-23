module RunCommands where

import           Control.Concurrent (threadDelay)
import           Control.Monad      (forM, forM_, mapM_, when)
import           Data.List          (maximumBy)
import qualified Data.Map           as M
import           Data.Yaml          (encodeFile)
import           System.Directory
import           System.Process

import           Config
import           DirectoryUtils
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
    let fullFp = fullExerciseFp (projectRoot config) (exercisesExt config) exInfo
    let name = exerciseName exInfo
    isNotDone <- fileContainsNotDone fullFp
    let printNameAndDots = do
          when (i < 10) (progPutStr config " ")
          progPutStr config (show i)
          progPutStr config ": "
          progPutStr config name
          progPutStr config $ replicate (maxNameSize - length name) '.'
    if isNotDone
      then withTerminalFailure $ printNameAndDots >> progPutStrLn config "...NOT DONE"
      else withTerminalSuccess $ printNameAndDots >> progPutStrLn config ".......DONE"

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
  let configPath = projectRoot `pathJoin` configFileName
  alreadyExists <- doesFileExist configPath
  when alreadyExists $ removeFile configPath
  let config = BaseConfig
                 (if null ghc then Nothing else Just ghc)
                 (if null stackPath then Nothing else Just stackPath)
  if null ghc && null stackPath
    then putStrLn "No configuration information given, will rely on defaults."
    else encodeFile configPath config >> putStrLn ("Saved configuration to " ++ configPath)

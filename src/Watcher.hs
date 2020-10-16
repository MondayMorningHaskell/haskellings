module Watcher where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.FSNotify
import qualified Data.Map as M

import ExerciseList (exerciseMap)
import Utils

watchExercises :: (FilePath, FilePath) -> IO ()
watchExercises (projectRoot, ghcPath) = do
  let conf = defaultConfig { confDebounce = Debounce 1 }
  withManagerConf conf $ \mgr -> do
    watchTree mgr (projectRoot ++ "/src/exercises") shouldCheckFile (processEvent (projectRoot, ghcPath))
    forever $ threadDelay 1000000

shouldCheckFile :: Event -> Bool
shouldCheckFile (Modified fp _ _) = isHaskellFile fp && M.member (haskellModuleName fp) exerciseMap
shouldCheckFile _ = False

-- This event should be a modification of one of our exercise files
processEvent :: (FilePath, FilePath) -> Event -> IO ()
processEvent configPaths (Modified fp _ _) = case M.lookup modName exerciseMap of
  Nothing -> putStrLn $ "Couldn't find exercise: " ++ modName
  Just exerciseInfo -> do
    putStrLn $ "Running exercise: " ++ modName
    compileExercise configPaths exerciseInfo
  where
    modName = haskellModuleName fp
processEvent _ _ = return ()

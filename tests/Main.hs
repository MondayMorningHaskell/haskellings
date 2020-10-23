import Control.Concurrent
import Data.List
import Data.Time
import System.Directory
import System.Exit
import System.IO
import Test.Hspec
import Test.HUnit

import Config
import ExerciseList
import Utils
import Watcher

main :: IO ()
main = do
  loadResult <- loadProjectRootAndGhc
  case loadResult of
    Left _ -> error "Unable to find project root or GHC 8.8.4!"
    Right paths -> do
      createDirectoryIfMissing True (fst paths ++ "/tests/test_gen")
      hspec $ describe "Basic Compile Tests" $ do
        compileTests1 paths
        compileTests2 paths
        watchTests paths

compileBeforeHook :: (FilePath, FilePath) -> FilePath -> FilePath -> IO (String, ExitCode)
compileBeforeHook (projectRoot, ghcPath) inFile outFile = do
  let fullFp = projectRoot ++ "/tests/test_gen/" ++ outFile
  outHandle <- openFile fullFp WriteMode
  let conf = ProgramConfig projectRoot ghcPath "/tests/exercises/" stdin outHandle stderr
  let exerciseInfo = ExerciseInfo "Test" "types" inFile
  resultExit <- compileExercise conf exerciseInfo
  hClose outHandle
  programOutput <- readFile fullFp
  return (programOutput, resultExit)

isFailureOutput :: String -> Expectation
isFailureOutput output = do
  let outputLines = lines output
  length outputLines `shouldSatisfy` (> 1)
  head outputLines `shouldSatisfy` (isPrefixOf "Couldn't compile :")

isSuccessOutput :: String -> Expectation
isSuccessOutput output = output `shouldSatisfy` (isPrefixOf "Successfully compiled :")

compileTests1 :: (FilePath, FilePath) -> Spec
compileTests1 paths = before (compileBeforeHook paths "Types1Bad.hs" "types1_bad.output") $
  describe "When running 'compileExercise' with non-compiling file" $
    it "Should indicate failure to compile and return a failing exit code" $ \(output, exit) -> do
      exit `shouldNotBe` ExitSuccess
      isFailureOutput output

compileTests2 :: (FilePath, FilePath) -> Spec
compileTests2 paths = before (compileBeforeHook paths "Types1Good.hs" "types1_good.output") $
  describe "When running 'compileExercise' with compiling file" $
    it "Should indicate successful compilation and return a success exit code" $ \(output, exit) -> do
      exit `shouldBe` ExitSuccess
      isSuccessOutput output








watchTestExercises :: [ExerciseInfo]
watchTestExercises =
  [ ExerciseInfo "Types1" "watcher_types" "Types1.hs"
  , ExerciseInfo "Types2" "watcher_types" "Types2.hs"
  ]

watchTests :: (FilePath, FilePath) -> Spec
watchTests paths = before (beforeWatchHook paths "watcher_tests_.out") $
  describe "When running watcher" $
    it "Should step the through the watch process in stages" $ \outputs -> do
      assertSequence expectedSequence (lines outputs)
  where
    expectedSequence =
      [ "Couldn't compile : Types1.hs"
      , "Successfully compiled : Types1.hs"
      , "This exercise compiles! Remove 'I AM NOT DONE' to proceed!"
      , "Successfully compiled : Types1.hs"
      , "Couldn't compile : Types2.hs"
      , "Successfully compiled : Types2.hs"
      , "This exercise compiles! Remove 'I AM NOT DONE' to proceed!"
      , "Successfully compiled : Types2.hs"
      , "Congratulations, you've completed all the exercises!"
      ]

assertSequence :: [String] -> [String] -> Expectation
assertSequence [] _ = return ()
assertSequence remaining [] = assertFailure $
  "Did not find all expected messages. Remaining: " ++ show remaining
assertSequence all@(expectedString : restExpected) (fileLine : restFile) =
  if expectedString == fileLine
    then assertSequence restExpected restFile
    else assertSequence all restFile

makeModifications :: [(FilePath, FilePath)] -> IO ()
makeModifications [] = return ()
makeModifications ((src, dst) : rest) = do
  threadDelay 1000000
  copyFile src dst
  getCurrentTime >>= setModificationTime dst
  threadDelay 1000000
  makeModifications rest

beforeWatchHook :: (FilePath, FilePath) -> FilePath -> IO String
beforeWatchHook (projectRoot, ghcPath) outFile = do
  -- Copy Original Files
  copyFile (addFullDirectory "Types1Orig.hs") fullDest1
  copyFile (addFullDirectory "Types2Orig.hs") fullDest2
  -- Build Configuration
  let fullFp = projectRoot ++ "/tests/test_gen/" ++ outFile
  outHandle <- openFile fullFp WriteMode
  let conf = ProgramConfig projectRoot ghcPath "/tests/exercises/" stdin outHandle stderr
  watchTid <- forkIO (runExerciseWatch conf watchTestExercises)
  -- Modify Files
  makeModifications modifications
  killThread watchTid
  hClose outHandle
  removeFile fullDest1
  removeFile fullDest2
  programOutput <- readFile fullFp
  return programOutput
  where
    addFullDirectory = (++) (projectRoot ++ "/tests/exercises/watcher_types/")
    fullDest1 = addFullDirectory "Types1.hs"
    fullDest2 = addFullDirectory "Types2.hs"
    modifications =
      [ (addFullDirectory "Types1Mod1.hs", fullDest1)
      , (addFullDirectory "Types1Mod2.hs", fullDest1)
      , (addFullDirectory "Types2Mod1.hs", fullDest2)
      , (addFullDirectory "Types2Mod2.hs", fullDest2)
      ]

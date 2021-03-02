import           Control.Concurrent
import           Data.List
import qualified Data.Map           as M
import           Data.Time
import           System.Directory
import           System.Exit
import           System.IO
import           Test.Hspec
import           Test.HUnit

import           Config
import           DirectoryUtils
import           ExerciseList
import           Utils
import           Watcher

main :: IO ()
main = do
  loadResult <- loadProjectRootAndGhc
  case loadResult of
    Left _ -> error "Unable to find project root or GHC 8.8.4!"
    Right paths -> do
      createDirectoryIfMissing True (fst paths `pathJoin` "tests" `pathJoin` "test_gen")
      hspec $ describe "Basic Compile Tests" $ do
        compileTests1 paths
        compileTests2 paths
        compileAndRunTestFail1 paths
        compileAndRunTestFail2 paths
        compileAndRunTestPass paths
        compileAndRunTestPass2 paths
        compileAndExecFail1 paths
        compileAndExecFail2 paths
        compileAndExecPass paths
        watchTests paths

compileBeforeHook :: (FilePath, FilePath) -> ExerciseInfo -> FilePath -> IO (String, RunResult)
compileBeforeHook (projectRoot, ghcPath) exInfo outFile = do
  let fullFp = projectRoot `pathJoin` "tests" `pathJoin` "test_gen" `pathJoin` outFile
  outHandle <- openFile fullFp WriteMode
  packageDb <- findStackPackageDb
  let conf = ProgramConfig projectRoot ghcPath packageDb "/tests/exercises/" stdin outHandle stderr M.empty
  resultExit <- compileExercise conf exInfo
  hClose outHandle
  programOutput <- readFile fullFp
  return (programOutput, resultExit)

isFailureOutput :: String -> Expectation
isFailureOutput output = do
  let outputLines = lines output
  length outputLines `shouldSatisfy` (> 1)
  head outputLines `shouldSatisfy` isPrefixOf "Couldn't compile :"

isRunFailureOutput :: String -> Expectation
isRunFailureOutput output = do
  let outputLines = lines output
  length outputLines `shouldSatisfy` (> 1)
  head outputLines `shouldSatisfy` isPrefixOf "Successfully compiled :"
  outputLines !! 1 `shouldSatisfy` isPrefixOf "Tests failed on exercise :"

isExecFailureOutput :: String -> Expectation
isExecFailureOutput output = do
  let outputLines = lines output
  length outputLines `shouldSatisfy` (== 4)
  head outputLines `shouldSatisfy` isPrefixOf "Successfully compiled :"
  outputLines !! 1 `shouldSatisfy` isPrefixOf "Unexpected output for exercise:"
  outputLines !! 2 `shouldBe` "Check the Sample Input and Sample Output in the file."
  outputLines !! 3 `shouldSatisfy` isPrefixOf "Then try running it for yourself with 'haskellings exec"

isSuccessOutput :: String -> Expectation
isSuccessOutput output = output `shouldSatisfy` isPrefixOf "Successfully compiled :"

isSuccessRunOutput :: String -> Expectation
isSuccessRunOutput output = do
  let outputLines = lines output
  length outputLines `shouldBe` 2
  head outputLines `shouldSatisfy` isPrefixOf "Successfully compiled :"
  outputLines !! 1 `shouldSatisfy` isPrefixOf "Successfully ran :"

isSuccessExecOutput :: String -> Expectation
isSuccessExecOutput output = do
  let outputLines = lines output
  length outputLines `shouldBe` 3
  head outputLines `shouldSatisfy` isPrefixOf "Successfully compiled :"
  outputLines !! 1 `shouldSatisfy` isPrefixOf "Successfully ran :"
  outputLines !! 2 `shouldSatisfy` isPrefixOf "You can run this code for yourself with 'haskellings exec"

compileTests1 :: (FilePath, FilePath) -> Spec
compileTests1 paths = before (compileBeforeHook paths exInfo "types1_bad.output") $
  describe "When running 'compileExercise' with non-compiling file" $
    it "Should indicate failure to compile and return a failing exit code" $ \(output, exit) -> do
      exit `shouldBe` CompileError
      isFailureOutput output
  where
    exInfo = ExerciseInfo "Types1Bad" "types" CompileOnly ""

compileTests2 :: (FilePath, FilePath) -> Spec
compileTests2 paths = before (compileBeforeHook paths exInfo "types1_good.output") $
  describe "When running 'compileExercise' with compiling file" $
    it "Should indicate successful compilation and return a success exit code" $ \(output, exit) -> do
      exit `shouldBe` RunSuccess
      isSuccessOutput output
  where
    exInfo = ExerciseInfo "Types1Good" "types" CompileOnly ""

compileAndRunTestFail1 :: (FilePath, FilePath) -> Spec
compileAndRunTestFail1 paths = before (compileBeforeHook paths exInfo "recursion1_bad1.output") $
  describe "When running 'compileExercise' with non-compiling and runnable file" $
    it "Should indicate failure to compile and return a failing exit code" $ \(output, exit) -> do
      exit `shouldBe` CompileError
      isFailureOutput output
  where
    exInfo = ExerciseInfo "Recursion1Bad1" "recursion" UnitTests ""

compileAndRunTestFail2 :: (FilePath, FilePath) -> Spec
compileAndRunTestFail2 paths = before (compileBeforeHook paths exInfo "recursion1_bad2.output") $
  describe "When running 'compileExercise' with compiling but incorrect file" $
    it "Should indicate test failures and return a failing exit code" $ \(output, exit) -> do
      isRunFailureOutput output
      exit `shouldBe` TestFailed
  where
    exInfo = ExerciseInfo "Recursion1Bad2" "recursion" UnitTests ""

compileAndRunTestPass :: (FilePath, FilePath) -> Spec
compileAndRunTestPass paths = before (compileBeforeHook paths exInfo "recursion1_good.output") $
  describe "When running 'compileExercise' with compiling and runnable file" $
    it "Should indicate successful compilation and return a success exit code" $ \(output, exit) -> do
      exit `shouldBe` RunSuccess
      isSuccessRunOutput output
  where
    exInfo = ExerciseInfo "Recursion1Good" "recursion" UnitTests ""

compileAndRunTestPass2 :: (FilePath, FilePath) -> Spec
compileAndRunTestPass2 paths = before (compileBeforeHook paths exInfo "recursion2.output") $
  describe "When running 'compileExercise' with compiling and runnable file that has a unit testing library" $
    it "Should indicate successful compilation and return a success exit code" $ \(output, exit) -> do
      exit `shouldBe` RunSuccess
      isSuccessRunOutput output
  where
    exInfo = ExerciseInfo "Recursion2" "recursion" UnitTests ""

compileAndExecFail1 :: (FilePath, FilePath) -> Spec
compileAndExecFail1 paths = before (compileBeforeHook paths exInfo "io_bad1.output") $
  describe "When running 'compileExercise' with a non-compiling executable exercise" $
    it "Should indicate failure to compile and return a failing exit code" $ \(output, exit) -> do
      exit `shouldBe` CompileError
      isFailureOutput output
  where
    exInfo = ExerciseInfo "IOBad1" "io" (Executable ["John"] (== ["Hello, please enter your name!", "Hello, John"])) ""

compileAndExecFail2 :: (FilePath, FilePath) -> Spec
compileAndExecFail2 paths = before (compileBeforeHook paths exInfo "io_bad1.output") $
  describe "When running 'compileExercise' with a non-compiling executable exercise" $
    it "Should indicate failure to compile and return a failing exit code" $ \(output, exit) -> do
      exit `shouldBe` TestFailed
      isExecFailureOutput output
  where
    exInfo = ExerciseInfo "IOBad2" "io" (Executable ["John"] (== ["Hello, please enter your name!", "Hello, John"])) ""

compileAndExecPass :: (FilePath, FilePath) -> Spec
compileAndExecPass paths = before (compileBeforeHook paths exInfo "io_good.output") $
  describe "When running 'compileExercise' with a passing executable exercise" $
    it "Should indicate successful compilation and return a success exit code" $ \(output, exit) -> do
      exit `shouldBe` RunSuccess
      isSuccessExecOutput output
  where
    exInfo = ExerciseInfo "IOGood" "io" (Executable ["John"] (== ["Hello, please enter your name!", "Hello, John"])) ""


-- Watcher Tests
watchTestExercises :: [ExerciseInfo]
watchTestExercises =
  [ ExerciseInfo "Types1" "watcher_types" CompileOnly "What type should you fill in for the variable?"
  , ExerciseInfo "Types2" "watcher_types" CompileOnly "What type can you fill in for the tuple?"
  ]

watchTests :: (FilePath, FilePath) -> Spec
watchTests paths = before (beforeWatchHook paths "watcher_tests_.out") $
  describe "When running watcher" $
    it "Should step the through the watch process in stages" $ \outputs -> assertSequence expectedSequence (lines outputs)
  where
    expectedSequence =
      [ "Couldn't compile : Types1.hs"
      , "What type should you fill in for the variable?"
      , "Successfully compiled : Types1.hs"
      , "This exercise succeeds! Remove 'I AM NOT DONE' to proceed!"
      , "Successfully compiled : Types1.hs"
      , "Couldn't compile : Types2.hs"
      , "Successfully compiled : Types2.hs"
      , "This exercise succeeds! Remove 'I AM NOT DONE' to proceed!"
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

makeModifications :: ProgramConfig -> [(FilePath, FilePath)] -> IO ()
makeModifications _ [] = return ()
makeModifications conf ((src, dst) : rest) = do
  threadDelay 1000000
  withFileLock dst conf $ do
    removeFile dst
    copyFile src dst
    getCurrentTime >>= setModificationTime dst
  threadDelay 1000000
  makeModifications conf rest

beforeWatchHook :: (FilePath, FilePath) -> FilePath -> IO String
beforeWatchHook (projectRoot, ghcPath) outFile = do
  -- Copy Original Files
  copyFile (addFullDirectory "Types1Orig.hs") fullDest1
  copyFile (addFullDirectory "Types2Orig.hs") fullDest2
  -- Build Configuration
  let fullFp = projectRoot `pathJoin` "tests" `pathJoin` "test_gen" `pathJoin` outFile
  let fullIn = projectRoot `pathJoin` "tests" `pathJoin` "watcher_tests.in"
  outHandle <- openFile fullFp WriteMode
  inHandle <- openFile fullIn ReadMode
  lock1 <- newEmptyMVar
  lock2 <- newEmptyMVar
  let locks = M.fromList [(fullDest1, lock1), (fullDest2, lock2)]
  let conf = ProgramConfig projectRoot ghcPath Nothing testExercisesDir inHandle outHandle stderr locks
  watchTid <- forkIO (runExerciseWatch conf watchTestExercises)
  -- Modify Files
  makeModifications conf modifications
  killThread watchTid
  hClose outHandle
  hClose inHandle
  removeFile fullDest1
  removeFile fullDest2
  readFile fullFp
  where
    testExercisesDir = makeRelative ("tests" `pathJoin` "exercises")
    watcherTypesDir = "tests" `pathJoin` "exercises" `pathJoin` "watcher_types"
    addFullDirectory = pathJoin (projectRoot `pathJoin` watcherTypesDir)
    fullDest1 = addFullDirectory "Types1.hs"
    fullDest2 = addFullDirectory "Types2.hs"
    modifications =
      [ (addFullDirectory "Types1Mod1.hs", fullDest1)
      , (addFullDirectory "Types1Mod2.hs", fullDest1)
      , (addFullDirectory "Types2Mod1.hs", fullDest2)
      , (addFullDirectory "Types2Mod2.hs", fullDest2)
      ]

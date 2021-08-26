import           Control.Concurrent
import           Control.Monad.Reader
import           Data.List
import qualified Data.Map                   as M
import           Data.Time
import           System.Directory
import           System.FilePath            ((</>))
import           System.IO
import           Test.Hspec
import           Test.HUnit

import           Haskellings.DirectoryUtils
import           Haskellings.LoadConfig
import           Haskellings.Processor
import           Haskellings.RunCommands
import           Haskellings.Types
import           Haskellings.Watcher

main :: IO ()
main = do
  loadResult <- loadBaseConfigPaths
  case loadResult of
    Left _ -> error "Unable to find project root or GHC 8.8.4!"
    Right paths@(root, _, _) -> do
      createDirectoryIfMissing True (root </> "tests" </> "test_gen")
      hspec $ describe "Basic Compile Tests" $ do
        compileTests1 paths
        compileTests2 paths
        compileAndRunTestFail1 paths
        compileAndRunTestFail2 paths
        compileAndRunTestPass paths
        compileAndRunTestPass2 paths
        compileAndExecFail1 paths
        compileAndExecFail2 paths
        compileAndExecFail3 paths
        compileAndExecPass paths
        watchTests paths
        listTest paths

compileBeforeHook :: (FilePath, FilePath, FilePath) -> ExerciseInfo -> FilePath -> IO (String, RunResult)
compileBeforeHook (projectRoot, ghcPath, packageDb) exInfo outFile = do
  let fullFp = projectRoot </> "tests" </> "test_gen" </> outFile
  outHandle <- openFile fullFp WriteMode
  let conf = ProgramConfig projectRoot ghcPath packageDb ("tests" </> "exercises") stdin outHandle stderr M.empty
  resultExit <- runReaderT (compileAndRunExercise exInfo) conf
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

isExecRunFailureOutput :: String -> Expectation
isExecRunFailureOutput output = do
  let outputLines = lines output
  length outputLines `shouldSatisfy` (> 4)
  head outputLines `shouldSatisfy` isPrefixOf "Successfully compiled :"
  outputLines !! 1 `shouldSatisfy` isPrefixOf "Encountered error running exercise:"
  last (init outputLines) `shouldBe` "Check the Sample Input and Sample Output in the file."
  last outputLines `shouldSatisfy` isPrefixOf "Then try running it for yourself with 'haskellings exec"

isExecTestFailureOutput :: String -> Expectation
isExecTestFailureOutput output = do
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

compileTests1 :: (FilePath, FilePath, FilePath) -> Spec
compileTests1 paths = before (compileBeforeHook paths exInfo "types1_bad.output") $
  describe "When running 'compileAndRunExercise' with non-compiling file" $
    it "Should indicate failure to compile and return a failing exit code" $ \(output, exit) -> do
      exit `shouldBe` CompileError
      isFailureOutput output
  where
    exInfo = ExerciseInfo "Types1Bad" "types" CompileOnly ""

compileTests2 :: (FilePath, FilePath, FilePath) -> Spec
compileTests2 paths = before (compileBeforeHook paths exInfo "types1_good.output") $
  describe "When running 'compileAndRunExercise' with compiling file" $
    it "Should indicate successful compilation and return a success exit code" $ \(output, exit) -> do
      exit `shouldBe` RunSuccess
      isSuccessOutput output
  where
    exInfo = ExerciseInfo "Types1Good" "types" CompileOnly ""

compileAndRunTestFail1 :: (FilePath, FilePath, FilePath) -> Spec
compileAndRunTestFail1 paths = before (compileBeforeHook paths exInfo "recursion1_bad1.output") $
  describe "When running 'compileAndRunExercise' with non-compiling and runnable file" $
    it "Should indicate failure to compile and return a failing exit code" $ \(output, exit) -> do
      exit `shouldBe` CompileError
      isFailureOutput output
  where
    exInfo = ExerciseInfo "Recursion1Bad1" "recursion" UnitTests ""

compileAndRunTestFail2 :: (FilePath, FilePath, FilePath) -> Spec
compileAndRunTestFail2 paths = before (compileBeforeHook paths exInfo "recursion1_bad2.output") $
  describe "When running 'compileAndRunExercise' with compiling but incorrect file" $
    it "Should indicate test failures and return a failing exit code" $ \(output, exit) -> do
      isRunFailureOutput output
      exit `shouldBe` TestFailed
  where
    exInfo = ExerciseInfo "Recursion1Bad2" "recursion" UnitTests ""

compileAndRunTestPass :: (FilePath, FilePath, FilePath) -> Spec
compileAndRunTestPass paths = before (compileBeforeHook paths exInfo "recursion1_good.output") $
  describe "When running 'compileAndRunExercise' with compiling and runnable file" $
    it "Should indicate successful compilation and return a success exit code" $ \(output, exit) -> do
      exit `shouldBe` RunSuccess
      isSuccessRunOutput output
  where
    exInfo = ExerciseInfo "Recursion1Good" "recursion" UnitTests ""

compileAndRunTestPass2 :: (FilePath, FilePath, FilePath) -> Spec
compileAndRunTestPass2 paths = before (compileBeforeHook paths exInfo "recursion2.output") $
  describe "When running 'compileAndRunExercise' with compiling and runnable file that has a unit testing library" $
    it "Should indicate successful compilation and return a success exit code" $ \(output, exit) -> do
      exit `shouldBe` RunSuccess
      isSuccessRunOutput output
  where
    exInfo = ExerciseInfo "Recursion2" "recursion" UnitTests ""

compileAndExecFail1 :: (FilePath, FilePath, FilePath) -> Spec
compileAndExecFail1 paths = before (compileBeforeHook paths exInfo "io_bad1.output") $
  describe "When running 'compileAndRunExercise' with a non-compiling executable exercise" $
    it "Should indicate failure to compile and return a failing exit code" $ \(output, exit) -> do
      exit `shouldBe` CompileError
      isFailureOutput output
  where
    exInfo = ExerciseInfo "IOBad1" "io" (Executable ["John"] (== ["Hello, please enter your name!", "Hello, John"])) ""

compileAndExecFail2 :: (FilePath, FilePath, FilePath) -> Spec
compileAndExecFail2 paths = before (compileBeforeHook paths exInfo "io_bad2.output") $
  describe "When running 'compileAndRunExercise' with a compiling executable exercise that errors." $
    it "Should indicate failure to compile and return a failing exit code" $ \(output, exit) -> do
      exit `shouldBe` TestFailed
      isExecRunFailureOutput output
  where
    exInfo = ExerciseInfo "IOBad2" "io" (Executable ["John"] (== ["Hello, please enter your name!", "Hello, John"])) ""

compileAndExecFail3 :: (FilePath, FilePath, FilePath) -> Spec
compileAndExecFail3 paths = before (compileBeforeHook paths exInfo "io_bad3.output") $
  describe "When running 'compileAndRunExercise' with a compiling executable exercise that doesn't pass the tests." $
    it "Should indicate failure to compile and return a failing exit code" $ \(output, exit) -> do
      exit `shouldBe` TestFailed
      isExecTestFailureOutput output
  where
    exInfo = ExerciseInfo "IOBad3" "io" (Executable ["John"] (== ["Hello, please enter your name!", "Hello, John"])) ""

compileAndExecPass :: (FilePath, FilePath, FilePath) -> Spec
compileAndExecPass paths = before (compileBeforeHook paths exInfo "io_good.output") $
  describe "When running 'compileAndRunExercise' with a passing executable exercise" $
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

watchTests :: (FilePath, FilePath, FilePath) -> Spec
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
  flip runReaderT conf $ withFileLock dst $ lift $ do
    removeFile dst
    copyFile src dst
    getCurrentTime >>= setModificationTime dst
  threadDelay 1000000
  makeModifications conf rest

beforeWatchHook :: (FilePath, FilePath, FilePath) -> FilePath -> IO String
beforeWatchHook (projectRoot, ghcPath, stackPackageDb) outFile = do
  -- Copy Original Files
  copyFile (addFullDirectory "Types1Orig.hs") fullDest1
  copyFile (addFullDirectory "Types2Orig.hs") fullDest2
  -- Build Configuration
  let fullFp = projectRoot </> "tests" </> "test_gen" </> outFile
  let fullIn = projectRoot </> "tests" </> "watcher_tests.in"
  outHandle <- openFile fullFp WriteMode
  inHandle <- openFile fullIn ReadMode
  lock1 <- newEmptyMVar
  lock2 <- newEmptyMVar
  let locks = M.fromList [(fullDest1, lock1), (fullDest2, lock2)]
  let conf = ProgramConfig projectRoot ghcPath stackPackageDb testExercisesDir inHandle outHandle stderr locks
  watchTid <- forkIO (runReaderT (runExerciseWatch watchTestExercises) conf)
  -- Modify Files
  makeModifications conf modifications
  killThread watchTid
  hClose outHandle
  hClose inHandle
  removeFile fullDest1
  removeFile fullDest2
  readFile fullFp
  where
    testExercisesDir = "tests" </> "exercises"
    watcherTypesDir = "tests" </> "exercises" </> "watcher_types"
    addFullDirectory = (</>) (projectRoot </> watcherTypesDir)
    fullDest1 = addFullDirectory "Types1.hs"
    fullDest2 = addFullDirectory "Types2.hs"
    modifications =
      [ (addFullDirectory "Types1Mod1.hs", fullDest1)
      , (addFullDirectory "Types1Mod2.hs", fullDest1)
      , (addFullDirectory "Types2Mod1.hs", fullDest2)
      , (addFullDirectory "Types2Mod2.hs", fullDest2)
      ]

-- Test 'list' commands

listTestExercises :: [ExerciseInfo]
listTestExercises =
  [ ExerciseInfo "Types1Bad" "types" CompileOnly ""
  , ExerciseInfo "Types1Good" "types" CompileOnly ""
  , ExerciseInfo "Recursion1Bad1" "recursion" UnitTests ""
  , ExerciseInfo "Recursion1Bad2" "recursion" UnitTests ""
  , ExerciseInfo "Recursion1Good" "recursion" UnitTests ""
  , ExerciseInfo "Recursion2" "recursion" UnitTests ""
  ]

listBeforeHook :: (FilePath, FilePath, FilePath) -> FilePath -> IO String
listBeforeHook (projectRoot, ghcPath, stackPackageDb) outFile = do
  let fullFp = projectRoot </> "tests" </> "test_gen" </> outFile
  outHandle <- openFile fullFp WriteMode
  let conf = ProgramConfig projectRoot ghcPath stackPackageDb ("tests" </> "exercises") stdin outHandle stderr M.empty
  runReaderT (listExercises' listTestExercises) conf
  hClose outHandle
  readFile fullFp

listTest :: (FilePath, FilePath, FilePath) -> Spec
listTest paths = before (listBeforeHook paths "list.output") $
  describe "When running 'listExercises'" $
    it "Should print the proper status" $ \output -> lines output `shouldBe` expectedOutput
  where
    expectedOutput =
      [ "Listing exercises...(must remove \"I AM NOT DONE\" comment to indicate as done)"
      , " 1: Types1Bad........NOT DONE"
      , " 2: Types1Good...........DONE"
      , " 3: Recursion1Bad1...NOT DONE"
      , " 4: Recursion1Bad2...NOT DONE"
      , " 5: Recursion1Good...NOT DONE"
      , " 6: Recursion2...........DONE"
      ]

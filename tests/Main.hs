import Data.List
import System.Directory
import System.Exit
import System.IO
import Test.Hspec

import Config
import Utils

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

compileBeforeHook :: (FilePath, FilePath) -> FilePath -> FilePath -> IO (String, ExitCode)
compileBeforeHook (projectRoot, ghcPath) inFile outFile = do
  let fullFp = projectRoot ++ "/tests/test_gen/" ++ outFile
  outHandle <- openFile fullFp WriteMode
  let conf = ProgramConfig projectRoot ghcPath "/tests/exercises/" stdin outHandle stderr
  let exerciseInfo = ("Test", "types", inFile)
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

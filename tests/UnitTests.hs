module Main where

import           Data.List                  (isSuffixOf)
import qualified Data.Sequence              as S
import           System.Directory
import           System.FilePath            (takeDirectory, takeFileName, (</>))
import           Test.Tasty
import           Test.Tasty.HUnit

import           Haskellings.Constants
import           Haskellings.DirectoryUtils
import           Haskellings.LoadConfig

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let testsRoot = findProjectRootBackwards currentDir </> "tests"
  defaultMain $ testGroup "Unit Tests"
    [ searchForDirectoryTests testsRoot
    , fpBFSTests testsRoot
    , snapshotPackagePredicateTests testsRoot
    ]
  where
    findProjectRootBackwards "" = error "Couldn't find project root!"
    findProjectRootBackwards dir = if takeFileName dir == projectRootDirName || takeFileName dir == ciProjectRootDirName
      then dir
      else findProjectRootBackwards (takeDirectory dir)

shouldReturn :: (Show a, Eq a) => IO a -> a -> Assertion
shouldReturn action expected = do
  actual <- action
  actual @?= expected

-- Make a directory within tests which has a few levels to look through.
searchForDirectoryTests :: FilePath -> TestTree
searchForDirectoryTests testsRoot = testGroup "searchForDirectory Tests"
  [ testCase "Search for directory 1" $
      searchForDirectoryContaining testsRoot "ghc" `shouldReturn` Just (testsRoot </> actual1)
  , testCase "Search for directory 2" $
      searchForDirectoryContaining testsRoot "8.6.2" `shouldReturn` Just (testsRoot </> actual2)
  , testCase "Search for directory 3" $
      searchForDirectoryContaining testsRoot "8.3.4" `shouldReturn` Nothing
  ]
  where
    actual1 = "directory_tests" </> "test1" </> "ghc"
    actual2 = "directory_tests" </> "test2" </> "8.6.2"

fpBFSTests :: FilePath -> TestTree
fpBFSTests testsRoot = testGroup "fpBFS Tests"
  [ testCase "fpBFS 1" $
      fpBFS pred1 (S.singleton testsRoot) `shouldReturn` Just (testsRoot </> actual1)
  , testCase "fpBFS 2" $
      fpBFS pred2 (S.singleton testsRoot) `shouldReturn` Just (testsRoot </> actual2)
  , testCase "fpBFS 3" $
      fpBFS pred3 (S.singleton testsRoot) `shouldReturn` Nothing
  , testCase "fpBFS 4" $
      fpBFS (return . ghcPred) (S.singleton root3) `shouldReturn` Just (testsRoot </> actual3)
  ]
  where
    pred1 fp = return ("ghc-8.8.4" `isSuffixOf` fp)
    pred2 fp = return ("ghc-8.6.2" `isSuffixOf` fp)
    pred3 fp = return ("ghc-8.3.4" `isSuffixOf` fp)
    actual1 = "directory_tests" </> "test1" </> "linux-x86_64-ghc-8.8.4"
    actual2 = "directory_tests" </> "test2" </> "windows-x86_64-ghc-8.6.2"
    root3   = testsRoot </> "directory_tests" </> "test3"
    actual3 = "directory_tests" </> "test3" </> "ghc-tinfo6-8.10.4"

snapshotPackagePredicateTests :: FilePath -> TestTree
snapshotPackagePredicateTests testsRoot = testGroup "snapshotPackagePredicate Tests"
  [ testCase "snapshotPackagePredicate 1" $ snapshotPackagePredicate path1 `shouldReturn` True
  , testCase "snapshotPackagePredicate 2" $ snapshotPackagePredicate path2 `shouldReturn` False
  ]
  where
    path1 = testsRoot </> "directory_tests" </> "package_test" </>
                "hash1" </> "8.10.4" </> "lib" </> "x86_64-linux-ghc-8.10.4"
    path2 = testsRoot </> "directory_tests" </> "package_test" </>
                "hash2" </> "8.10.4" </> "lib" </> "x86_64-linux-ghc-8.10.4"

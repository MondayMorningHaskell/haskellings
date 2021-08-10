module Main where

import           Data.List        (isSuffixOf)
import qualified Data.Sequence    as S
import           System.Directory
import           System.FilePath  (takeDirectory, takeFileName)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Config
import           DirectoryUtils

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let testsRoot = findProjectRootBackwards currentDir `pathJoin` "tests"
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
      searchForDirectoryContaining testsRoot "ghc" `shouldReturn` Just (testsRoot `pathJoin` actual1)
  , testCase "Search for directory 2" $
      searchForDirectoryContaining testsRoot "8.6.2" `shouldReturn` Just (testsRoot `pathJoin` actual2)
  , testCase "Search for directory 3" $
      searchForDirectoryContaining testsRoot "8.3.4" `shouldReturn` Nothing
  ]
  where
    actual1 = "directory_tests" `pathJoin` "test1" `pathJoin` "ghc"
    actual2 = "directory_tests" `pathJoin` "test2" `pathJoin` "8.6.2"

fpBFSTests :: FilePath -> TestTree
fpBFSTests testsRoot = testGroup "fpBFS Tests"
  [ testCase "fpBFS 1" $
      fpBFS pred1 (S.singleton testsRoot) `shouldReturn` Just (testsRoot `pathJoin` actual1)
  , testCase "fpBFS 2" $
      fpBFS pred2 (S.singleton testsRoot) `shouldReturn` Just (testsRoot `pathJoin` actual2)
  , testCase "fpBFS 3" $
      fpBFS pred3 (S.singleton testsRoot) `shouldReturn` Nothing
  , testCase "fpBFS 4" $
      fpBFS (return . ghcPred) (S.singleton root3) `shouldReturn` Just (testsRoot `pathJoin` actual3)
  ]
  where
    pred1 fp = return ("ghc-8.8.4" `isSuffixOf` fp)
    pred2 fp = return ("ghc-8.6.2" `isSuffixOf` fp)
    pred3 fp = return ("ghc-8.3.4" `isSuffixOf` fp)
    actual1 = "directory_tests" `pathJoin` "test1" `pathJoin` "linux-x86_64-ghc-8.8.4"
    actual2 = "directory_tests" `pathJoin` "test2" `pathJoin` "windows-x86_64-ghc-8.6.2"
    root3   = testsRoot `pathJoin` "directory_tests" `pathJoin` "test3"
    actual3 = "directory_tests" `pathJoin` "test3" `pathJoin` "ghc-tinfo6-8.10.4"

snapshotPackagePredicateTests :: FilePath -> TestTree
snapshotPackagePredicateTests testsRoot = testGroup "snapshotPackagePredicate Tests"
  [ testCase "snapshotPackagePredicate 1" $ snapshotPackagePredicate path1 `shouldReturn` True
  , testCase "snapshotPackagePredicate 2" $ snapshotPackagePredicate path2 `shouldReturn` False
  ]
  where
    path1 = testsRoot `pathJoin` "directory_tests" `pathJoin` "package_test" `pathJoin`
                "hash1" `pathJoin` "8.10.4" `pathJoin` "lib" `pathJoin` "x86_64-linux-ghc-8.10.4"
    path2 = testsRoot `pathJoin` "directory_tests" `pathJoin` "package_test" `pathJoin`
                "hash2" `pathJoin` "8.10.4" `pathJoin` "lib" `pathJoin` "x86_64-linux-ghc-8.10.4"

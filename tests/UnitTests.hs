module Main where

import           Data.List        (isSuffixOf)
import qualified Data.Sequence    as S
import           System.Directory
import           Test.Tasty
import           Test.Tasty.HUnit

import           Config
import           DirectoryUtils

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let testsRoot = findProjectRootBackwards currentDir `pathJoin` "tests"
  defaultMain $ testGroup "Unit Tests"
    [ basenameTests
    , dropDirectoryLevelTests
    , makeRelativeTests
    , pathJoinTests
    , searchForDirectoryTests testsRoot
    , fpBFSTests testsRoot
    , snapshotPackagePredicateTests testsRoot
    ]
  where
    findProjectRootBackwards "" = error "Couldn't find project root!"
    findProjectRootBackwards dir = if basename dir == projectRootDirName || basename dir == ciProjectRootDirName
      then dir
      else findProjectRootBackwards (dropDirectoryLevel dir)

shouldReturn :: (Show a, Eq a) => IO a -> a -> Assertion
shouldReturn action expected = do
  actual <- action
  actual @?= expected

basenameTests :: TestTree
basenameTests = testGroup "basename Tests"
  [ testCase "Unix case" $ basename "/home/haskellings/src/DirectoryUtils.hs" @?= "DirectoryUtils.hs"
  , testCase "Unix relative end" $ basename "/home/haskellings/src/DirectoryUtils.hs/" @?= "DirectoryUtils.hs"
  , testCase "Windows case" $ basename "C:\\home\\haskellings\\src\\DirectoryUtils.hs" @?= "DirectoryUtils.hs"
  , testCase "Windows relative end" $ basename "C:\\home\\haskellings\\src\\DirectoryUtils.hs\\" @?= "DirectoryUtils.hs"
  ]

dropDirectoryLevelTests :: TestTree
dropDirectoryLevelTests = testGroup "dropDirectoryLevel Tests"
  [ testCase "Simple Unix case" $ dropDirectoryLevel "/home/user/.stack/snapshots/lib/linux-x86_64-ghc-8.8.4" @?= "/home/user/.stack/snapshots/lib"
  , testCase "Unix relative end" $ dropDirectoryLevel "/home/user/.stack/snapshots/lib/linux-x86_64-ghc-8.8.4/" @?= "/home/user/.stack/snapshots/lib"
  , testCase "Simple Windows case" $ dropDirectoryLevel "C:\\sr\\snapshots\\778da73\\lib\\windows-x86_64-ghc-8.8.4" @?= "C:\\sr\\snapshots\\778da73\\lib"
  , testCase "Windows relative end" $ dropDirectoryLevel "C:\\sr\\snapshots\\778da73\\lib\\windows-x86_64-ghc-8.8.4\\" @?= "C:\\sr\\snapshots\\778da73\\lib"
  ]

makeRelativeTests :: TestTree
makeRelativeTests = testGroup "makeRelative Tests"
  [ testCase "Simple Unix case" $ makeRelativeUnix "Lib.hs" @?= "/Lib.hs"
  , testCase "Compound Unix case" $ makeRelativeUnix "src/Lib.hs" @?= "/src/Lib.hs"
  , testCase "Unix relative begin" $ makeRelativeUnix "/Lib.hs" @?= "/Lib.hs"
  , testCase "Simple Windows case" $ makeRelativeWindows "Lib.hs" @?= "\\Lib.hs"
  , testCase "Compound Windows case" $ makeRelativeWindows "src\\Lib.hs" @?= "\\src\\Lib.hs"
  , testCase "Windows relative begin" $ makeRelativeWindows "\\Lib.hs" @?= "\\Lib.hs"
  ]

pathJoinTests :: TestTree
pathJoinTests = testGroup "pathJoin Tests"
  [ testCase "Windows non-relative x non-relative" $ pathJoinWindows "C:\\haskellings" "src\\Lib.hs" @?= "C:\\haskellings\\src\\Lib.hs"
  , testCase "Windows non-relative x relative" $ pathJoinWindows "C:\\haskellings" "\\src\\Lib.hs" @?= "C:\\haskellings\\src\\Lib.hs"
  , testCase "Windows relative x non-relative" $ pathJoinWindows "C:\\haskellings\\" "src\\Lib.hs" @?= "C:\\haskellings\\src\\Lib.hs"
  , testCase "Windows relative x relative" $ pathJoinWindows "C:\\haskellings\\" "\\src\\Lib.hs" @?= "C:\\haskellings\\src\\Lib.hs"
  , testCase "Unix non-relative x non-relative" $ pathJoinUnix "/home/haskellings" "src/Lib.hs" @?= "/home/haskellings/src/Lib.hs"
  , testCase "Unix non-relative x relative" $ pathJoinUnix "/home/haskellings" "/src/Lib.hs" @?= "/home/haskellings/src/Lib.hs"
  , testCase "Unix relative x non-relative" $ pathJoinUnix "/home/haskellings/" "src/Lib.hs" @?= "/home/haskellings/src/Lib.hs"
  , testCase "Unix relative x relative" $ pathJoinUnix "/home/haskellings/" "/src/Lib.hs" @?= "/home/haskellings/src/Lib.hs"
  ]

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
  ]
  where
    pred1 fp = return ("ghc-8.8.4" `isSuffixOf` fp)
    pred2 fp = return ("ghc-8.6.2" `isSuffixOf` fp)
    pred3 fp = return ("ghc-8.3.4" `isSuffixOf` fp)
    actual1 = "directory_tests" `pathJoin` "test1" `pathJoin` "linux-x86_64-ghc-8.8.4"
    actual2 = "directory_tests" `pathJoin` "test2" `pathJoin` "windows-x86_64-ghc-8.6.2"

snapshotPackagePredicateTests :: FilePath -> TestTree
snapshotPackagePredicateTests testsRoot = testGroup "snapshotPackagePredicate Tests"
  [ testCase "snapshotPackagePredicate 1" $ snapshotPackagePredicate path1 `shouldReturn` True
  , testCase "snapshotPackagePredicate 2" $ snapshotPackagePredicate path2 `shouldReturn` False
  ]
  where
    path1 = testsRoot `pathJoin` "directory_tests" `pathJoin` "package_test" `pathJoin`
                "hash1" `pathJoin` "8.8.4" `pathJoin` "lib" `pathJoin` "x86_64-linux-ghc-8.8.4"
    path2 = testsRoot `pathJoin` "directory_tests" `pathJoin` "package_test" `pathJoin`
                "hash2" `pathJoin` "8.8.4" `pathJoin` "lib" `pathJoin` "x86_64-linux-ghc-8.8.4"

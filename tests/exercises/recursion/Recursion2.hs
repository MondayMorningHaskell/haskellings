import Test.Tasty
import Test.Tasty.HUnit

sumList :: [Int] -> Int
sumList [] = 0
sumList (a : as) = a + sumList as

main :: IO ()
main = defaultMain $
  testCase "List Recursion" $ do
    sumList [1, 2, 3, 4] @?= 10
    sumList [2, 3, 5, 6] @?= 16

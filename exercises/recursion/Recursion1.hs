-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

sumList :: [Int] -> Int
sumList = undefined

main :: IO ()
main = defaultMain $
  testCase "List Recursion" $ do
    sumList [1, 2, 3, 4] @?= 10
    sumList [2, 3, 5, 6] @?= 16

-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- The 'zip' function is very useful for combining lists in an element-by-element way.
  It takes two lists and produces a list of tuples.

zip :: [a] -> [b] -> [(a, b)]

zip [1, 2, 3] ["Hi", "Bye", "Good"] = [(1, "Hi"), (2, "Bye"), (3, "Good")]

- If either list is shorter, the remaining elements of the other list are omitted.

zip [1, 2] ["Hi", "Bye", "Good"] = [(1, "Hi"), (2, "Bye")]
zip [1, 2, 3] ["Hi", "Bye"] = [(1, "Hi"), (2, "Bye")]

- It is a very useful utility, especially combines with items from the last couple
  exercises, like ranges, infinite lists and list comprehensions!

-}


-- Given two lists, match them up element by element and
-- return tuples containing the sum, product, and difference of each pair.
-- sumProductDifference [4, 3] [1, 2] = [(5, 4, 3), (5, 6, 1)]
sumProductDifference :: [Int] -> [Int] -> [(Int, Int, Int)]
sumProductDifference = ???

-- Given a list of strings, make a new list where each String has been
-- appended with its index within the list.
-- addIndices ["Hello", "Bye"] = ["Hello0", "Bye1"]
addIndices :: [String] -> [String]
addIndices = ???

main :: IO ()
main = defaultMain $ testGroup "Lists4" $
  [ testCase "sumProductDifference 1" $ sumProductDifference [] [] @?= []
  , testCase "sumProductDifference 2" $ sumProductDifference [1, 2] [] @?= []
  , testCase "sumProductDifference 3" $ sumProductDifference [1] [3, 4] @?= [(4, 3, -2)]
  , testCase "sumProductDifference 4" $ sumProductDifference [1, 2, 3, 4] [5, 6, 7, 8] @?=
      [(6, 5, -4), (8, 12, -4), (10, 21, -4), (12, 32, -4)]
  , testCase "sumProductDifference 5" $ sumProductDifference [13, 10, 11] [5, 6, 7, 8] @?=
      [(18, 65, 8), (16, 60, 4), (18, 77, 4)]
  , testCase "addIndices 1" $ addIndices [] @?= []
  , testCase "addIndices 2" $ addIndices ["Hello"] @?= ["Hello0"]
  , testCase "addIndices 3" $ addIndices ["Hello", "Goodbye"] @?= ["Hello0", "Goodbye1"]
  , testCase "addIndices 4" $ addIndices ["1", "0", "2"] @?= ["10", "01", "22"]
  ]

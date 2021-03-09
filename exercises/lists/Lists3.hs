-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- *List comprehensions* allow us to a new way to create lists.
  A list comprehension is an expression with brackets that
  has up to 3 components:

1. The data source
2. An optional filter 
3. The resulting data

- Here's one example, which uppercases all the words which are
  5 characters or shorter:

words = ["Hello", "Bye", "Farewell", "Ciao"]
shortWords = [map toUpper word | word <- words, length word <= 5]

shortWords = ["HELLO", "BYE", "CIAO"]

1. The data source is that we are considering each individual element in the
   'words' list:

[...| word <- words, ...]

2. The filter is that we only want to consider words that are shorter:

[... | ... , length word <= 5]

3. The result is that we uppercase the word:

[map toUpper word | ... ]

- So putting it all together, we get:

[map toUpper word | word <- words, length word <= 5]

- We can also take elements from *two different lists*. This will compute every
  pairwise sum and difference from two different lists we provide. Notice that
  we apply no filter this time.

[ x + y | x <- [1..10], y <- [11..20]]

-}

-- TODO: Implement these functions using list comprehensions!

-- Implement addMod3Is2, except now use a list comprehension.
addMod3Is2 :: [Int] -> [Int]
addMod3Is2 = ???

-- Take every pairwise product of the numbers, as long as their
-- sum is less than 30.
smallPairwiseProducts :: [Int] -> [Int] -> [Int]
smallPairwiseProducts = ???

main :: IO ()
main = defaultMain $ testGroup "Lists3" $
  [ testCase "addMod3Is2 1" $ addMod3Is2 [] @?= []
  , testCase "addMod3Is2 2" $ addMod3Is2 [2] @?= [5]
  , testCase "addMod3Is2 3" $ addMod3Is2 [2, 4, 5, 8, 9] @?= [5, 8, 11]
  , testCase "Pairwise Products 1" $ smallPairwiseProducts [] [] @?= []
  , testCase "Pairwise Products 2" $ smallPairwiseProducts [] [1] @?= []
  , testCase "Pairwise Products 3" $ smallPairwiseProducts [2] [] @?= []
  , testCase "Pairwise Products 4" $ smallPairwiseProducts [2] [1] @?= [2]
  , testCase "Pairwise Products 5" $ smallPairwiseProducts [2] [29] @?= []
  , testCase "Pairwise Products 6" $ smallPairwiseProducts [1, 2, 3] [1, 2, 3] @?=
      [1, 2, 3, 2, 4, 6, 3, 6, 9]
  , testCase "Pairwise Products 7" $ smallPairwiseProducts [1, 2, 3] [27, 28, 29] @?=
      [27, 28, 54]
  ]

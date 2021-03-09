-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- Remember 'map'? This was a *higher order function* that took two inputs:

1. A function for transforming values
2. A list to operate on

map :: (a -> b) -> [a] -> [b]

- Using 'map', we could potentially spare ourselves from having to write a
  recursive function on lists.

- There are many other functions like this! First up is *filter*:
  This function will take a *predicate function* that evaluates
  each element of the list on some condition. The resulting list
  has all the original elements which return true for the function!

filter :: (a -> Bool) -> [a] -> [a]

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

filter isEven [1, 2, 3, 4] -> [2, 4]

- Then there's 'foldr', a *folding* helper. This has three inputs:

foldr :: (a -> b -> b) -> b -> [a] -> b

- These are the arguments, in order:

1. The folding function
2. An initial accumulated value
3. Our input list

- A fold works a bit like tail recursion. It produces a final type 'b',
  and it also takes an initial accumulated value. Then it peels the 'a' elements
  off our list one-by-one and feeds them into the folding function, applying
  against the total accumulated value.

- As a simple example, consider taking the minimum of some integers
  We'll go through it step-by-step.

foldr min 9999 [5, 7, 2, 9, 1]

1. min 9999 5 -> 5
2. min 5 7 -> 5
3. min 5 2 -> 2
4. min 2 9 -> 2
5. min 2 1 -> 1 -- < This is our final answer!

-}

-- TODO:
-- Re-implement some of our recursion functions, but use
-- higher order helper functions instead!

addMod3Is2 :: [Int] -> [Int]
addMod3Is2 = ???

sumList :: [Int] -> Int
sumList = ???

-- Return true if every element is 'True'!
-- (The empty list should return 'True'!)
allTrue :: [Bool] -> Bool
allTrue bs = ???

main :: IO ()
main = defaultMain $ testGroup "Lists1" $
  [ testCase "addMod3Is2 1" $ addMod3Is2 [] @?= []
  , testCase "addMod3Is2 2" $ addMod3Is2 [2] @?= [5]
  , testCase "addMod3Is2 3" $ addMod3Is2 [2, 4, 5, 8, 9] @?= [5, 8, 11]
  , testCase "sumList 1" $ sumList [] @?= 0
  , testCase "sumList 2" $ sumList [5, -3] @?= 2
  , testCase "sumList 3" $ sumList [6, 8, 9, -2, 10] @?= 31
  , testCase "allTrue 1" $ allTrue [] @?= True
  , testCase "allTrue 2" $ allTrue [False] @?= False
  , testCase "allTrue 3" $ allTrue [False, True, True] @?= False
  , testCase "allTrue 4" $ allTrue [True, True, True, True] @?= True
  ]

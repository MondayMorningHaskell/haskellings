-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- Lists are inherently recursive data structures, as we'll explore soon.
  This means recursion is often a good solution for problems involving lists.
  As an example, let's take the sum of a list of Ints

sumList :: [Int] -> Int
...

- If the list is empty, the sum is clearly 0. This is our *base case*.

sumList :: [Int] -> Int
sumList [] = 0
...

- Now we can use a pattern match to deconstruct our list into its first element
  and then *a smaller list*. We can make the recursive call on that list, and
  add the first element!

sumList :: [Int] -> Int
sumList [] = 0                    -- < BASE CASE
sumList (a : as) = a + sumList as -- < RECURSIVE CASE

- Recursion is a 4-step process:

1. Determine the base case
2. Break larger cases into one piece and then a smaller case (or two)
3. Make the recursive call on the smaller case(s)
4. Combine the original piece and the recursive answer(s)

-}

-- TODO: Solve these recursion problems involving lists!

-- Take only the numbers of the input that are equal to "2 mod 3" (2, 5, 8, etc.)
-- Then add 3 to each of them!
-- addMod3Is2 [2, 3, 4, 8] = [5, 10]
addMod3Is2 :: [Int] -> [Int]
addMod3Is2 = ???

-- Take only the 'even' index elements of the list (the second, fourth, sixth, etc.)
-- evens [1, 5, 7, 0, 3, 2, 2, 3] = [5, 0, 2, 3]
evens :: [Int] -> [Int]
evens = ???

main :: IO ()
main = defaultMain $ testGroup "Recusion2" $
  [ testCase "addMod3Is2 1" $ addMod3Is2 [] @?= []
  , testCase "addMod3Is2 2" $ addMod3Is2 [2] @?= [5]
  , testCase "addMod3Is2 3" $ addMod3Is2 [2, 4, 5, 8, 9] @?= [5, 8, 11]
  , testCase "Evens 1" $ evens [] @?= []
  , testCase "Evens 2" $ evens [1] @?= []
  , testCase "Evens 3" $ evens [1, 2, 3] @?= [2]
  , testCase "Evens 4" $ evens [1, 2, 3, 4] @?= [2, 4]
  , testCase "Evens 5" $ evens [1, 2, 3, 4, 5, 6, 7] @?= [2, 4, 6]
  ]

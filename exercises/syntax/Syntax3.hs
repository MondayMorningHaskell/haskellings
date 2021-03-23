-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- Pattern Matching is a another powerful tool to branch our code behavior based
  on the values or structure of our input elements.

- Instead of assigning a name to our inputs, we can use a value on the left side
  to indicate what the behavior of the function should be when the input has
  that value. We do this for multiple different values, essentially defining
  multiple versions of our function depending on the input.


-}

-- Here's an example where we have different behavior when the input
-- boolean is True vs. False
addOrSubtract :: Bool -> Int -> Int -> Int
addOrSubtract True x y = x + y
addOrSubtract False x y = x - y

-- Here's our we might write 'countTrue' with 2 inputs:
-- We don't need the input values in the last pattern, so we use
-- underscores ('_') as placeholders. If we needed it in the function
-- definition, we could assign names like normal.
countTrue :: Bool -> Bool -> Int
countTrue True True = 2
countTrue False False = 0
countTrue _ _ = 1

-- TODO:

-- Rewrite numberString from last time, but use a pattern match instead of guards!
numberString :: Word -> String
numberString = ???

-- If the input number is 0-3, return the first, corresponding number of elements
-- in the list. e.g. takeN 0 [1,2,3,4] = [], takeN 2 [1,2,3,4] = [1,2]
-- If the input number is larger, return the whole list.
-- You can assume the input has at least 4 elements.
takeN :: Word -> [Int] -> [Int]
takeN = ???

-- Testing Code
main :: IO ()
main = defaultMain $ testGroup "Syntax3" $
  [ testCase "numberString 1" $ numberString 1 @?= "One"
  , testCase "numberString 2" $ numberString 4 @?= "Four"
  , testCase "numberString 3" $ numberString 6 @?= "Too many!"
  , testCase "takeN 0 " $ takeN 0 [1, 2, 3, 4, 5] @?= []
  , testCase "takeN 1 " $ takeN 1 [1, 2, 3, 4, 5] @?= [1]
  , testCase "takeN 2 " $ takeN 2 [1, 2, 3, 4, 5] @?= [1, 2]
  , testCase "takeN 3 " $ takeN 3 [1, 2, 3, 4, 5] @?= [1, 2, 3]
  , testCase "takeN 6 " $ takeN 6 [1, 2, 3, 4, 5] @?= [1, 2, 3, 4, 5]
  ]

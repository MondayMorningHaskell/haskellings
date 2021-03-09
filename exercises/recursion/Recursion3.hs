-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- *Tail recursion* is a special kind of recursion. Our goal is that the
  recursive call is the *last* thing our recursive case actually has to
  do. That is, there is no additional work to combine a previous piece
  after a recursive answer is retrieved.

- Haskell code is optimized so that tail recursion is more memory efficient,
  which allows us to call our functions on larger inputs.

- We track the work we've done by making a helper function that takes an
  *accumulation argument*. In our 'sumList' example, we'll have this argument
  represent the sum of elements we've already seen. Then we call our helper
  function with a trivial first input:

sumList :: [Int] -> Int
sumList xs = sumListTail xs 0 -- < Initial accumulator is 0
  where
    sumListTail :: [Int] -> Int -> Int
    sumListTail [] accum = accum -- < BASE CASE - Return accumulator
    sumListTail (x : xs) accum = sumListTail xs (x + accum) -- < RECURSIVE CASE
-}

-- TODO:

-- Write a tail-recursive function to reverse the list!
-- (Note: In real code, you can rely on the 'reverse' library function)
reverseList :: [a] -> [a]
reverseList = ???

-- Given a list, return a tuple of two sub-lists.
-- The first is the "even" elements like you did in the last exercise!
-- The second is the "odd" elements.
-- Do this tail recursively!
-- evenOdds [1, 5, 7, 0, 3, 2, 2, 3] = ([5, 0, 2, 3], [1, 7, 3, 2])
evenOdds :: [a] -> ([a], [a])
evenOdds = ???

main :: IO ()
main = defaultMain $ testGroup "Recusion3" $
  [ testCase "Reverse 1" $ reverseList [] @?= ([] :: [Int])
  , testCase "Reverse 2" $ reverseList [1] @?= [1]
  , testCase "Reverse 3" $ reverseList [1, 2, 3, 4, 5, 6] @?= [6, 5, 4, 3, 2, 1]
  , testCase "Reverse 4" $ reverseList ["Hello", "Goodbye", "There"] @?= ["There", "Goodbye", "Hello"]
  , testCase "evenOdds 1" $ evenOdds [] @?= ([], [] :: [Int])
  , testCase "evenOdds 2" $ evenOdds [1] @?= ([], [1])
  , testCase "evenOdds 3" $ evenOdds [1, 2, 3] @?= ([2], [1, 3])
  , testCase "evenOdds 4" $ evenOdds [1, 2, 3, 4] @?= ([2, 4], [1, 3])
  , testCase "evenOdds 5" $ evenOdds [1, 2, 3, 4, 5, 6, 7] @?= ([2, 4, 6], [1, 3, 5, 7])
  ]

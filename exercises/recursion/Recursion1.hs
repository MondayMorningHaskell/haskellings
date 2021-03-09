-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- *Recursion* is a fundamental concept in Haskell.
  A *recursive* function calls itself within its definition.
  This might seem like it will continue infinitely.
  If you aren't careful, it will!

- The key to recursion is having a *base case*. When your input
  is sufficiently simple, the answer is easy and obvious!
  When the input is larger or more complicated, you do a little
  bit of work, and then call the function again on a *smaller, less complicated input*.

- Positive integer division is a simple function we can write recursively.
  When the dividend (first input) is smaller than the divisor (second),
  the answer is 0. This is our *base case*.

quotient :: Word -> Word -> Word 
quotient dividend divisor = if dividend < divisor
  then 0   -- < BASE CASE
  else ...

- What if it's larger? We can subtract the divisor from the dividend.
  Then we can get the quotient of that *smaller* case, and add 1!
  This is the *recursive case*, since we call 'quotient' again.

quotient :: Word -> Word -> Word
quotient dividend divisor = if dividend < divisor
  -- BASE CASE
  then 0
  -- RECURSIVE CASE
  else 1 + (quotient (dividend - divisor) divisor)

- Since the recursive case has a smaller input, it will eventually reach the
  base case and our call stack will terminate!

-}

-- TODO: Complete these recursive functions!

-- Calculator the factorial as 1 * 2 * ... * n
-- factorial 1 = 1
-- factorial 2 = 2 -- (1 * 2)
-- factorial 3 = 6 -- (1 * 2 * 3)
-- ...
factorial :: Int -> Int
factorial = ???

-- Calculate the 'specialDistance' to 1, according to these rules:
-- The distance from 1 to itself is 0
-- The distance from 0 to 1 is 1
-- If the input n is even, add 1 to the distance to n / 2.
-- If the input n is odd, add 1 to the distance to 3n + 1.
--  NOTE: The actual library function for integer division is 'quot'.
specialDistance :: Word -> Word
specialDistance = ???

main :: IO ()
main = defaultMain $ testGroup "Recusion1" $
  [ testCase "Factorial 1" $ factorial 1 @?= 1
  , testCase "Factorial 2" $ factorial 2 @?= 2
  , testCase "Factorial 3" $ factorial 3 @?= 6
  , testCase "Factorial 4" $ factorial 4 @?= 24
  , testCase "Factorial 5" $ factorial 5 @?= 120
  , testCase "Special Distance 0" $ specialDistance 0 @?= 1
  , testCase "Special Distance 1" $ specialDistance 1 @?= 0
  , testCase "Special Distance 2" $ specialDistance 2 @?= 1
  , testCase "Special Distance 3" $ specialDistance 3 @?= 7
  , testCase "Special Distance 4" $ specialDistance 19 @?= 20
  , testCase "Special Distance 5" $ specialDistance 128 @?= 7
  ]

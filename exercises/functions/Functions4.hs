-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- The math functions we've been using so far (+, -, *) are a special type of
  functions called "operators".

- These are two-argument functions defined as a series of
  non-alpha-numeric characters between parentheses.

- Here are (simplified) type signatures for addition and multiplication:

(+) :: Int -> Int -> Int
(*) :: Int -> Int -> Int

- When a function is defined like this, we can use it in an "infix" way, placing
  the operator (without parentheses) between the arguments.

  5 + 4 = 9
  3 * 6 = 18

- But we can also treat them like normal functions by keeping the parentheses and
  placing both arguments after the operator

(+) 5 4 = 9
(*) 3 6 = 18

-}

-- We can combine the ideas of partial application and operators!
add5 :: Int -> Int
add5 = (+) 5

-- TODO: Define this function, like the example above.
multiplyBy6 :: Int -> Int
multiplyBy6 = undefined

-- TODO: Define your own operator, (%=%)!
-- This should take two integers and produce another integer.
-- It should multiply the inputs together, subtract the second from the first,
-- and then add the result. 
-- 5 %=% 6 = 29

-- Testing Code
main :: IO ()
main = defaultMain $ testGroup "Functions4" $
  [ testCase "multiplyBy6 1" $ multiplyBy6 5 @?= 30
  , testCase "multiplyBy6 2" $ multiplyBy6 (-3) @?= (-18)
  , testCase "%=% 1" $ (5 %=% 6) @?= 29
  , testCase "%=% 2" $ (30 %=% 14) @?= 436
  ]

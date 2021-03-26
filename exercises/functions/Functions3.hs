-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- Suppose we have a function that takes two arguments. If we only apply
  *one* of the arguments, we get a new function that takes only one argument.

-}

multiplyBy3AndAdd :: Int -> Int -> Int
multiplyBy3AndAdd x y = y * 3 + x

-- We take the function above and apply a single argument.
-- Thus we still have to take one more "Int"!
multiplyBy3Add5 :: Int -> Int
multiplyBy3Add5 = multiplyBy3AndAdd 5

-- multiplyBy3AndAdd 4 5 = 19
-- multiplyBy3AndAdd 10 -3 = 1
-- multiplyBy3Add5 5 = 20
-- multiplyBy3Add5 -3 = -4

-- TODO: Fill in these functions!

-- Take two tuples.
-- Multiply the first elements of each tuple together.
-- Then multiply the second elements together as well.
-- Add the results.
multiplyAndAdd :: (Int, Int) -> (Int, Int) -> Int
multiplyAndAdd = ???

-- Take a tuple of two Ints.
-- Multiply the first value by 3 and the second by 4. Then add the results.
multiplyBy3And4AndAdd :: ???
multiplyBy3And4AndAdd = ???

-- Testing Code
main :: IO ()
main = defaultMain $ testGroup "Functions3" $
  [ testCase "multiplyAndAdd 1" $ multiplyAndAdd (6, -1) (14, 3) @?= 81
  , testCase "multiplyAndAdd 2" $ multiplyAndAdd (-4, -5) (-1, 3) @?= -11
  , testCase "multiplyBy3And4AndAdd 1" $ multiplyBy3And4AndAdd (14, 3) @?= 54
  , testCase "multiplyBy3And4AndAdd 2" $ multiplyBy3And4AndAdd (-1, 3) @?= 9
  ]

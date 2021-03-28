-- I AM NOT DONE

import Data.Char (toUpper, toLower)
import Test.Tasty
import Test.Tasty.HUnit

{-

- There are many functions already out there that we can call on our different types.
  Here are a few examples:

1. head :: [a] -> a
   returns the first element of any non-empty list ("a" is a generic type)
   head [1, 2, 3] = 1

2. tail :: [a] -> [a]
   returns everything _but_ the first element of a non-empty list
   tail [1, 2, 3] = [2, 3]

3. fst :: (a, b) -> a
   returns the first element of a tuple
   fst (True, []) = True

4. snd :: (a, b) -> b
   returns the second element of a tuple
   snd (True, []) = []

5. toUpper :: Char -> Char
   Uppercases an ASCII character
   toUpper 'a' = 'A'

6. toLower :: Char -> Char
   Lowercases an ASCII character
   toUpper 'Z' = 'z'

-}

-- TODO: Fill in these functions!

-- Add the heads of the two inputs together. Return this as the first element
-- of the tuple. Then you should also return the tail of each list.
addHeads :: ([Int], [Int]) -> (Int, [Int], [Int])
addHeads = ???

-- Produce a new tuple containing the three *second* elements of each of
-- the input tuples.
takeSeconds :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int, Int)
takeSeconds = ???

-- Capitalize each of the two characters.
capitalize :: (Char, Char) -> (Char, Char)
capitalize = ???

-- Testing Code, You can ignore this:
main :: IO ()
main = defaultMain $ testGroup "Functions2" $
  [ testCase "Add Heads 1" $ addHeads ([1, 2, 3], [4, 5, 6]) @?= (5, [2, 3], [5, 6])
  , testCase "Add Heads 2" $ addHeads ([-1, 6, 9], [3, -3, -10]) @?= (2, [6, 9], [-3, -10])
  , testCase "Take Seconds 1" $ takeSeconds (4, 5) (1, 2) (-1, -2) @?= (5, 2, -2)
  , testCase "Take Seconds 2" $ takeSeconds (100, 121) (-3, -4) (50, -67) @?= (121, -4, -67)
  , testCase "Capitalize 1" $ capitalize ('a', 'b') @?= ('A', 'B')
  , testCase "Capitalize 2" $ capitalize ('f', 'q') @?= ('F', 'Q')
  ]

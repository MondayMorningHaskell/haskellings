-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- There are many other operators on other types as well.


- We can use operators for boolean "and" and "or":

(&&) :: Bool -> Bool -> Bool
True && True = True
False && True = False

(||) :: Bool -> Bool -> Bool
False || False = False
True || False = True

There's also "not", which isn't an operator:
not :: Bool -> Bool
not True = False


- There are two important operators we can use on lists.

- The "append" operator will combine two lists into one:
(++) :: [a] -> [a] -> [a]
[1, 2, 3] ++ [4, 5, 6] = [1, 2, 3, 4, 5, 6]

- The "concat" operator will place push a new element onto
  the front of a list:
(:) :: a -> [a] -> [a]
1 : [2, 3] = [1, 2, 3]

-}

-- TODO: Fill in these functions!

-- This should take any boolean value and return True!
-- (There are two easy ways to do this, try both!)
trueSink :: Bool -> Bool
trueSink = ???

-- This should take any boolean value and return False!
falseSink :: Bool -> Bool
falseSink = ???

-- Return True if and only if all three inputs are true.
tripleAnd :: Bool -> Bool -> Bool -> Bool
tripleAnd = ???

-- Take two lists. Remove the first element from each list and add them together.
-- The first output should be like the first list except with this new sum at the
-- start instead of the original value. The second output should be the tail of the
-- original second input.
-- addToFirstList [1, 2] [9, 8] = ([10, 2], [8])
addToFirstList :: [Int] -> [Int] -> ([Int], [Int])
addToFirstList = ???

-- Testing Code
main :: IO ()
main = defaultMain $ testGroup "Functions5" $
  [ testCase "trueSink 1" $ trueSink True @?= True
  , testCase "trueSink 2" $ trueSink False @?= True
  , testCase "falseSink 1" $ falseSink True @?= False
  , testCase "falseSink 2" $ falseSink False @?= False
  , testCase "tripleAnd 1" $ tripleAnd True True True @?= True
  , testCase "tripleAnd 2" $ tripleAnd True True False @?= False
  , testCase "addToFirstList 1" $ addToFirstList [1, 2] [7, 8] @?= ([8, 2], [8])
  , testCase "addToFirstList 2" $ addToFirstList [34, 16, 14] [-13, 7, 2] @?= ([21, 16, 14], [7, 2])
  ]

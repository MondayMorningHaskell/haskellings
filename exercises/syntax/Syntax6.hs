-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- A 'where' clause is not the only way to define intermediate values.
  You can also use the keywords 'let' and 'in', like so:

sumEarlyDigits :: Bool -> [Int] -> Int
sumEarlyDigits bool ls =
  let tail1 = tail ls
      second = head tail1
      solution1 = head ls + second
      solution2 = second + head (tail tail1)
  in  if bool then solution1 else solution2

- Follow 'let' with all the expressions you want to define, and once you're
  done you use 'in' to complete the function in terms of your expressions.

sumProducts :: Int -> Int -> Int -> Int
sumProducts x y z =
  let  prod1 = x * y
       prod2 = y * z
       prod3 = x * z
  in   prod1 + prod2 + prod3

- The same rules about ordering apply in a 'let' statement! This circular
  dependency is still bad!

badSum :: Int -> Int -> Int -> Int
badSum x y z = prod1 + prod2 + prod3 + prod4
 let   prod2 = prod1 + (y * z) -- < Can use prod1 even though it's defined "after".
       prod1 = x * y
       prod3 = prod4 + x
       prod4 = y * prod3 -- < This is a problem! prod3 and prod4 depend on each other!
  in   prod1 + prod2 + prod3 + prod4
-}

-- TODO: Re-do these functions from the last part, only use 'let' instead of 'where'!

-- Take the sum of each pairwise product of inputs.
sumPairProducts :: (Int, Int, Int, Int, Int, Int) -> Int
sumPairProducts = ???

-- Take the sum of corresponding elements of the tuples, but only include each
-- pair when the corresponding bool is true.
-- e.g. sumTuples (True, False, False) (1, 2, 3) (4, 5, 6) = 5
--      sumTuples (True, False, True)  (1, 2, 3) (4, 5, 6) = 14
sumTuples :: (Bool, Bool, Bool) -> (Int, Int, Int) -> (Int, Int, Int) -> Int
sumTuples = ???

-- Testing Code
main :: IO ()
main = defaultMain $ testGroup "Syntax6" $
  [ testCase "sumPairProducts 1" $ sumPairProducts (1, 2, 3, 4, 5, 6) @?= 175
  , testCase "sumPairProducts 2" $ sumPairProducts (8, 2, -3, 4, -5, 7) @?= 1
  , testCase "sumTuples 1" $ sumTuples (True, True, True) (1, 2, 3) (4, 5, 6) @?= 21
  , testCase "sumTuples 2" $ sumTuples (True, False, True) (1, 2, 3) (4, 5, 6) @?= 14
  , testCase "sumTuples 3" $ sumTuples (False, True, False) (1, 2, 3) (4, 5, 6) @?= 7
  ]

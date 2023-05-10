import Test.Tasty
import Test.Tasty.HUnit

{-

- So far our functions have been very simple. So we've been able to write the whole
  result on a single line without re-using sub-expressions. But often this isn't the case.
  Suppose we want to use some part of our answer multiple times. It would make our code
  more readable to be able to "save" this value:

sumEarlyDigits :: Bool -> [Int] -> Int
sumEarlyDigits bool ls = if bool
  then head ls + head (tail ls)
  else head (tail ls) + head (tail (tail ls))

- A 'where' clause allows us to save expressions like 'tail ls' here.
  We indicate this section of our function with the 'where' keyword
  and add a series of expression definitions, which we can then use in our function.
  Notice how we can use 'tail1' within another variable definition!

sumEarlyDigits :: Bool -> [Int] -> Int
sumEarlyDigits bool ls = if bool
  then head ls + second
  else second + head (tail tail1)
  where
    tail1 = tail ls
    second = head tail1

- A 'where' clause can also allow you to make code more readable, even if
  sub-expressions aren't re-used.

sumProducts :: Int -> Int -> Int -> Int
sumProducts x y z = prod1 + prod2 + prod3
  where
    prod1 = x * y
    prod2 = y * z
    prod3 = x * z

- The order in which 'where' statements are defined *does not matter*. However,
  you must make sure to not create a circular dependency between your definitions!

badSum :: Int -> Int -> Int -> Int
badSum x y z = prod1 + prod2 + prod3 + prod4
  where
    prod2 = prod1 + (y * z) -- < Can use prod1 even though it's defined "after".
    prod1 = x * y
    prod3 = prod4 + x
    prod4 = y * prod3 -- < This is a problem! prod3 and prod4 depend on each other!
-}

-- TODO: Use 'where' clauses to complete these functions!

-- Take the sum of each pairwise product of inputs.
sumPairProducts :: (Int, Int, Int, Int, Int, Int) -> Int
sumPairProducts (i1,i2,i3,i4,i5,i6) = res
  where
    res = sum [r1, r2, r3, r4, r5]
      where 
      r1 = (i1*i2)+(i1*i3)+(i1*i4)+(i1*i5)+(i1*i6)
      r2 = (i2*i3)+(i2*i4)+(i2*i5)+(i2*i6)
      r3 = (i3*i4)+(i3*i5)+(i3*i6)
      r4 = (i4*i5)+(i4*i6)
      r5 = (i5*i6)


-- Take the sum of corresponding elements of the tuples, but only include each
-- pair when the corresponding bool is true.
-- e.g. sumTuples (True, False, False) (1, 2, 3) (4, 5, 6) = 5
--      sumTuples (True, False, True)  (1, 2, 3) (4, 5, 6) = 14
sumTuples :: (Bool, Bool, Bool) -> (Int, Int, Int) -> (Int, Int, Int) -> Int
sumTuples (b1,b2,b3) (x1,x2,x3) (y1,y2,y3) = s1 + s2 + s3
  where
    s1 = if b1 then x1+y1 else 0
    s2 = if b2 then x2+y2 else 0
    s3 = if b3 then x3+y3 else 0

-- Testing Code
main :: IO ()
main = defaultMain $ testGroup "Syntax5" $
  [ testCase "sumPairProducts 1" $ sumPairProducts (1, 2, 3, 4, 5, 6) @?= 175
  , testCase "sumPairProducts 2" $ sumPairProducts (8, 2, -3, 4, -5, 7) @?= 1
  , testCase "sumTuples 1" $ sumTuples (True, True, True) (1, 2, 3) (4, 5, 6) @?= 21
  , testCase "sumTuples 2" $ sumTuples (True, False, True) (1, 2, 3) (4, 5, 6) @?= 14
  , testCase "sumTuples 3" $ sumTuples (False, True, False) (1, 2, 3) (4, 5, 6) @?= 7
  ]

-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- Once you understand Functors, you can move on to *Applicative Functors*,
  often shortened as *Applicatives*. The name comes from the term
  "function *application*".

- A Functor stores information, and we apply functions over the information
  in the structure. But applicatives have tools that allow us to store
  the function itself in the structure and apply the stored function based
  on the structure's rules.

- Let's start with the class definition. This has two functions, instead
  of Functor, which only had 1:

class Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

- The first function, 'pure', tells us how to wrap an element in the
  structure in the most basic way.
- The second function, the "apply operator" takes a transformation within
  the structure, the structure containing the first type, and performs the
  transformation over the whole structure.

- Notice the difference with 'fmap'. With an applicative, the function itself
  is wrapped.

(<$>) ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b

- Maybe, Either, and List are Applicative Functors, in addition to being Functors!
  The instances for Maybe and Either are intuitive. We only create a "success" if
  both inputs are wrapped as success.

instance Applicative Maybe where
  pure a = Just a
  Just f <*> Just a = Just (f a)
  _ <*> _ = Nothing

instance Applicative (Either e) where
  pure a = Right a
  Right f <*> Right a = Right (f a)
  Right f <*> Left e = Left e
  Left e <*> _ = Left e

- With a List, the applicative is defined as a comprehension! We apply every
  function to every element in the second list!

instance Applicative [] where
  pure a = [a]
  fs <*> xs = [f x | f <- fs, x <- xs]

-}

safeSquareRoot :: Double -> Maybe Double
safeSquareRoot x = if x < 0 then Nothing else Just (sqrt x)

-- TODO: Fill in the following functions using applicatives!

-- Return the sum of the square roots of the two inputs, using
-- 'safeSquareRoot' to check for negatives.
sumOfSquareRoots :: Double -> Double -> Maybe Double
sumOfSquareRoots = ???

-- Generate all combinations of sums between the first list and the second list.
-- (avoid using a list comprehension)
generateSums :: [Int] -> [Int] -> [Int]
generateSums = ???

-- Given a list of operations and two lists, generate all combinations of
-- those operations with each pair of numbers from the two lists.
-- generateAllResults [(+), (*)] [1, 2] [3, 4] -> [4, 5, 5, 6, 3, 4, 6, 8]
generateAllResults :: [Int -> Int -> Int] -> [Int] -> [Int] -> [Int]
generateAllResults = ???

main :: IO ()
main = defaultMain $ testGroup "Applicatives" $
  [ testCase "sumOfSquareRoots 1" $ sumOfSquareRoots 4.0 9.0 @?= Just 5.0
  , testCase "sumOfSquareRoots 2" $ sumOfSquareRoots (-4.0) 9.0 @?= Nothing
  , testCase "sumOfSquareRoots 3" $ sumOfSquareRoots (-4.0) (-9.0) @?= Nothing
  , testCase "sumOfSquareRoots 4" $ sumOfSquareRoots 4.0 (-9.0) @?= Nothing
  , testCase "sumOfSquareRoots 5" $ sumOfSquareRoots 4.0 0.0 @?= Just 2.0
  , testCase "generateSums 1" $ generateSums [] [] @?= []
  , testCase "generateSums 2" $ generateSums [] [4, 5, 6] @?= []
  , testCase "generateSums 3" $ generateSums [4, 5, 6] [] @?= []
  , testCase "generateSums 4" $ generateSums [4, 5, 6] [2] @?= [6, 7, 8]
  , testCase "generateSums 5" $ generateSums [1, 2, 3] [4, 5, 6] @?= [5, 6, 7, 6, 7, 8, 7, 8, 9]
  , testCase "generateSums 6" $ generateSums [10, 8, 11, 6] [3, 4, -6] @?= [13, 14, 4, 11, 12, 2, 14, 15, 5, 9, 10, 0]
  , testCase "generateAllResults 1" $ generateAllResults ([] :: [Int -> Int -> Int]) [1, 2] [3, 4] @?= []
  , testCase "generateAllResults 2" $ generateAllResults [(+) :: Int -> Int -> Int, (*)] [] [3, 4] @?= []
  , testCase "generateAllResults 3" $ generateAllResults [(+)] [1, 2] [3, 4] @?= [4, 5, 5, 6]
  , testCase "generateAllResults 4" $ generateAllResults [(+), (*)] [1, 2] [3, 4] @?= [4, 5, 5, 6, 3, 4, 6, 8]
  , testCase "generateAllResults 5" $ generateAllResults [(+), (*), (-)] [10] [3, 4, 5] @?= [13, 14, 15, 30, 40, 50, 7, 6, 5]
  ]

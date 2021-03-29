-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-
- In Haskell, functions that take more than one argument are usually
  *curried*, meaning the function takes one argument and returns a new function
  that takes the second argument. 

- This is also reflected in the type signature of multi-argument functions:

f :: String -> Int -> Bool

- In the above example, f is a function that takes a String and returns 
  a function from Int to Bool. 

- In order to make this more explicit, we could write:

f ::  String -> (Int -> Bool)

- However, as the function arrow associates to the right, the parentheses
  can be (and usually are) omitted.
  
- The standard library defines a *higher order function* `curry`.
  
- This functions takes a single input argument: a function 
  that takes a pair (a, b) as input.

- The output of this function is a *new function* that takes its inputs separately
  (a -> b -> c), rather than as a tuple/pair.
  
curry :: ((a, b) -> c) -> a -> b -> c

- The inverse of `curry` is called `uncurry`.

- `uncurry` takes as its input a *curried* function and returns a *new function*
  which takes its arguments as a tuple/pair.

uncurry :: (a -> b -> c) -> (a, b) -> c

-}

-- The manhattan distance of two coordinate points is defined as 
-- the distance in x + the distance in y. 
-- This function takes the xdistance and ydistance and calculates the manhattan distance:


manhattanDistance :: Int -> Int -> Int
manhattanDistance xdist ydist = abs xdist + abs ydist

-- TODO:

-- Let's imagine that a pair, (Int, Int) represents a 2D point.
-- Define a function that takes the 2D point and calculates its manhattan
-- distance from the origin (0, 0).
-- Use `uncurry` and the manhattanDistance defined above.

manhattanDistancePoint :: (Int, Int) -> Int
manhattanDistancePoint = ???

-- This function takes a pair of booleans and returns a boolean 
-- corresponding to their logical AND

andPair :: (Bool, Bool) -> Bool
andPair (b1, b2) = b1 && b2

-- TODO:

-- Give this function a type signature to make it compile

andCurried :: ???
andCurried = curry andPair

-- Testing Code, You can ignore this:
main :: IO ()
main = defaultMain $ testGroup "Curry" $
  [ testCase "Manhattan Distance 1" $ manhattanDistancePoint (4, 6) @?= 10
  , testCase "Manhattan Distance 2" $ manhattanDistancePoint (2, -5) @?= 7
  , testCase "Manhattan Distance 3" $ manhattanDistancePoint (-3, -9) @?= 12
  , testCase "andCurried 1" $ andCurried True False @?= False
  , testCase "andCurried 2" $ andCurried True True @?= True
  ]

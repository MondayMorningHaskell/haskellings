module Curry1 where

-- I AM NOT DONE

{-
- In Haskell, functions that take more than one argument are usually
  curried, meaning the function takes one argument and returns another function that takes the second argument. 
  
- The standard library defines a *higher order function* *curry* which 
  takes a function from a pair (a,b) -> c as an argument and returns a function that takes a and returns a function b -> c.
  
curry :: ((a, b) -> c) -> a -> b -> c

- The inverse of *curry* is called *uncurry*.

uncurry :: (a -> b -> c) -> (a, b) -> c

-}

-- The manhatten distance of two coordinat points is defined as the distance in x + the distance in y. 
-- This function takes the xdistance and ydistance and calculates the manhatten distance:

manhattenDistance :: Int -> Int -> Int
manhattenDistance = (+)

-- The function manhattenDistance is written in *point-free style*
-- For an explanation of *point-free style*, see:
-- https://wiki.haskell.org/Pointfree

-- If you prefer, you could define it like this instead:

-- manhattenDistance :: Int -> Int -> Int
-- manhattenDistance xdist ydist = xdist + ydist

-- TODO:

-- Define a *type synonym* 'Point' for a pair (Int, Int).
-- Define a function that takes the Point and calculates its manhatten
-- distance from the origin (0, 0).
-- use *uncurry* and the manhattenDistance defined above

manhattenDistancePoint :: Point -> Int
manhattenDistancePoint = ???

-- This function takes a pair of booleans and returns a boolean 
-- corresponding to their logical AND

andPair :: (Bool, Bool) -> Bool
andPair (b1, b2) = b1 && b2

-- TODO:

-- Give this function a type signature to make it compile

andCurried :: ???
andCurried = curry andPair

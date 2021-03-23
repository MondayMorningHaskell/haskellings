-- I AM NOT DONE

import Data.Char (toUpper)

import Test.Tasty
import Test.Tasty.HUnit

{-

- A function can potentially take any expression as an input.
  Remember though that functions are also expressions!

- This means we can write a function that takes a different function *as an input*.
  We call this a *higher order function*.

- The most simple of these is "map", which transforms every element of a list.

map :: (a -> b) -> [a] -> [b]

- The first input is a function between two types (they can be the same type).
  The second input is the list of items

- **Lambda** syntax allows us to define a function as an expression in the
  middle of our code! This is like writing a normal function, except prefixing
  with a backslash and then listing the arguments, followed by an arrow!
  This technique allows you to use custom functions with other higher order
  functions, like `map`.

altAdder :: Int -> Int -> Int
altAdder = \a b -> a + b

-}

-- So for example, we make a function that transforms integers by doubling them.
double :: Int -> Int
double = (*) 2

-- Then we make a function that makes a new list which doubles every
-- element of its input.
doubleList :: [Int] -> [Int]
doubleList xs = map double xs
-- We could also just do: map ((*) 2) xs
-- With **lambda** syntax, we would have: map (\x -> 2 * x) xs

-- TODO: Fill in these functions using map!

-- Flip the boolean value of each input
flipBools :: [Bool] -> [Bool]
flipBools input = ???

-- Uppercase all the letters in this word!
capitalizeWord :: String -> String
capitalizeWord input = ???

-- TODO: Create your own higher order function, doubleAndApply.
-- The input function should take a single integer
-- and produce a tuple of three ints (Int, Int, Int)
-- The doubleAndApply function should then take an extra Int input,
-- double it, and then apply the function.
doubleAndApply :: ???
doubleAndApply = undefined

-- Testing Code
main :: IO ()
main = defaultMain $ testGroup "Functions6" $
  [ testCase "flipBools 1" $ flipBools [True, False, True] @?= [False, True, False]
  , testCase "flipBools 2" $ flipBools [False, False, False, False] @?= [True, True, True, True]
  , testCase "capitalizeWord 1" $ capitalizeWord "Hello" @?= "HELLO"
  , testCase "capitalizeWord 2" $ capitalizeWord "abcdefghzyx" @?= "ABCDEFGHZYX"
  , testCase "doubleAndApply 1" $ doubleAndApply (\i -> (i+1, i+2, i+3)) 5 @?= (11, 12, 13)
  , testCase "doubleAndApply 2" $ doubleAndApply (\i -> (i*2, i*3, i*4)) 5 @?= (20, 30, 40)
  ]

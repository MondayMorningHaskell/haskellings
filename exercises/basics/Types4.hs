module Types4 where

{-
- You can combine multiple values of the SAME type in a *List*.

- You can define a list with a comma separated list of values within *brackets*.

- It's type signature has the type of the elements within brackets.

- A list can have any number of elements, even 0.
-}

list1 :: [Int]
list1 = [1, 2, 3, 4]

emptyList :: [Float]
emptyList = []

boolList :: [Bool]
boolList = [True]

-- TODO: Fill in these lists with appropriate elements
--       (and fix type signatures as necessary)
ints :: [Int]
ints = [1..3]

floatingVals :: [Double]
floatingVals = [2.3, 3.5, 8.5, 10.1, 13.3, -42.1]

characters :: [Char]
characters = "asd"

unsignedInts :: [Word]
unsignedInts = [2]

-- TODO: Lists cannot have elements of different types!
--       Fix this list below.
mixedList :: [Double]
mixedList = [2, 3, 5, 6.7]


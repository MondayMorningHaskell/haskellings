import Data.List (sort)

import Test.Tasty
import Test.Tasty.HUnit

{-

- The 'Read' and 'Ord' typeclass are two other useful typeclasses that can
  be easily derived most of the time.

- 'Read' is the "inverse" of 'Show'. It allows us to create a type from a String

read :: String -> a

simpleInts :: [Int]
simpleInts = map read ["1", "2", "3"] -- > [1, 2, 3]

- 'Ord' allows us to provide an "ordering" on objects. That is, we can use
  operators like <, >, >=, and <=. The 'minimal complete definition' requires
  implementing '(<=)' or a 'compare' function. The latter takes two objects
  and produces an "Ordering".

-- Greater Than, Less Than, Equal
data Ordering = GT | LT | EQ

compare :: a -> a -> Ordering

compare 1 2 -> LT
compare "Hello" "Hello" -> EQ
compare "Hello" "Goodbye" -> GT

- So far, all of our constrained functions have had a single input type.
  Note however, that we can have multiple variable types with different
  constraints!

readAndCompare :: (Read a, Eq b) => (a -> b) -> String -> b -> Bool
readAndCompare f input expected = f (read input) == expected

-}

-- TODO:

-- Here we have two different 'Adult' types. Define a different 'Ord'
-- instance for each one. The first should order people by using
-- first name *and then* last name. The second should reverse this,
-- ordering by last name *and then* first name.
-- Try deriving the instance to see which behavior is the default!
data Adult1 = Adult1 String String Int
  deriving (Show, Eq)

instance Ord Adult1 where
  compare (Adult1 firstName1 lastName1 _) (Adult1 firstName2 lastName2 _) = if compare firstName1 firstName2 /= EQ then compare firstName1 firstName2 else compare lastName1 lastName2

data Adult2 = Adult2 String String Int
  deriving (Show, Eq, Read)

instance Ord Adult2 where
  compare (Adult2 firstName1 lastName1 _) (Adult2 firstName2 lastName2 _) = if compare lastName1 lastName2 /= EQ then compare lastName1 lastName2 else compare firstName1 firstName2 


-- Derive both 'Ord' and 'Read' for this type.
newtype InterestRate = InterestRate Double
  deriving (Eq, Show)

instance Ord InterestRate where
  compare (InterestRate r1) (InterestRate r2) = compare r1 r2

-- instance Read InterestRate where
--   readsPrec _ input = [(InterestRate r, rest) | (r, rest) <- reads input]

instance Read InterestRate where
  readsPrec _ input = do
    let (interestRateStr, rest) = span (/= ' ') input

    -- Check if the input starts with "InterestRate"
    if interestRateStr == "InterestRate"
      then case reads (dropWhile (== ' ') rest) of
             [(rate, "")] -> [(InterestRate rate, "")]
             _ -> []
      else []

-- This function should take two tuples, of variable types (a, b)
-- The 'a' type represents the person, the 'b' type represents
-- their interest rate. Whoever has the higher rate, return a string with
-- "'{p}' has a higher interest rate!", replacing {p} with 'show'-ing the person.
-- Fill in the type signature!
returnHigherInterestRate :: (Ord a, Show a, Ord b) => (a, b) -> (a, b) -> String
returnHigherInterestRate (p1, i1) (p2, i2)
  | compare i1 i2 == GT = "'" ++ show p1 ++ "' has a higher interest rate!"
  | compare i1 i2 == LT = "'" ++ show p2 ++ "' has a higher interest rate!"
  | otherwise = "they both earn the same"

-- Test Code

main :: IO ()
main = defaultMain $ testGroup "Typeclasses3" $
  [ testCase "Ordering Adults 1" $ sort [Adult1 "Zach" "Whittaker" 31, Adult1 "John" "Smith" 45, Adult1 "Thomas" "Allen" 46] @?=
      [Adult1 "John" "Smith" 45, Adult1 "Thomas" "Allen" 46, Adult1 "Zach" "Whittaker" 31]
  , testCase "Ordering Adults 2" $ sort [Adult2 "Zach" "Whittaker" 31, Adult2 "John" "Smith" 45, Adult2 "Thomas" "Allen" 46] @?=
      [Adult2 "Thomas" "Allen" 46, Adult2 "John" "Smith" 45, Adult2 "Zach" "Whittaker" 31]
  , testCase "Read Interest Rate" $ map read ["InterestRate 0.5", "InterestRate 0.3", "InterestRate 0.788"] @?= [InterestRate 0.5, InterestRate 0.3, InterestRate 0.788]
  , testCase "Higher Interest Rate 1" $ returnHigherInterestRate ("John", 0.3) ("Tom", 0.03) @?=
      "'\"John\"' has a higher interest rate!"
  , testCase "Higher Interest Rate 1" $ returnHigherInterestRate (Adult1 "John" "Smith" 15, InterestRate 0.07) (Adult1 "Tom" "Allen" 18, InterestRate 0.1) @?=
      "'Adult1 \"Tom\" \"Allen\" 18' has a higher interest rate!"
  ]

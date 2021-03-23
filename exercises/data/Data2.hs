-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- Haskell data types can also have multiple constructors! When we want
  this, we separate the constructors with a vertical bar character '|'.

- We've already seen an example of this in the "Bool" type.
  'True' and 'False' are its two constructors.

data Bool = True | False

- This makes it easy for us to let our data take different forms with
  different associated data.

data Calculation =
  RawResult Double |
  LoggedResult Double String

- You can branch on the constructor of your input with pattern matching
  and case statements:

doubleCalculation :: Calculation -> Double
doubleCalculation (RawResult x) = 2 * x
doubleCalculation (LoggedResult x _) = 2 * x

-}

-- TODO

-- First define an "Occupation" type with many different constructors
-- for what a person's job could be. You should at least include
-- 'Engineer' and 'Lawyer' as constructors. You don't need any extra data
-- for each constructor, like with Bool!

-- Take your two types from the last part and combine them into a single
-- 'Person' type with two constructors. Now use your new "Occupation" type
-- for the Adult's job instead of a string.

adult1 :: Person
adult1 = Adult "John" "Smith" 45 Lawyer

adult2 :: Person
adult2 = Adult "Jane" "Smith" 39 Engineer

child1 :: Person
child1 = Child "Christopher" 9 4

child2 :: Person
child2 = Child "Stephanie" 12 6

-- For adults, return their first and last name appended, with a space in between.
-- For children, just return their first name.
giveFullName :: Person -> String
giveFullName = undefined

-- Testing Code
main :: IO ()
main = defaultMain $ testGroup "Syntax1" $
  [ testCase "giveFullName 1" $ giveFullName adult1 @?= "John Smith"
  , testCase "giveFullName 2" $ giveFullName adult2 @?= "Jane Smith"
  , testCase "giveFullName 3" $ giveFullName child1 @?= "Christopher"
  , testCase "giveFullName 4" $ giveFullName child2 @?= "Stephanie"
  ]

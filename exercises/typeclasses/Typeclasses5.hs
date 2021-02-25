-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- Typeclasses can have dependencies on one another. When defining a typeclass,
  you can give a dependency in much the same way a function can be constrained.
  The simplest example of this is that 'Ord' depends on 'Eq'. We can only order
  elements if they might be equal to one another:

class (Eq a) => Ord a where
  ...

- We just give the dependency, then the sign '=>', and then our class definition
  as usual. This allows you to use functions from the "parent" class in functions
  constrained by your new, dependent class.

printEquals :: (Ord a) => a -> a -> String
printEquals x y = if x == y -- < Can use (==) because of implicit Eq constraint
  then ...

-}

-- TODO:

-- Define a class called Debuggable
-- This class should depend on 'Eq', 'Show', and 'Read'
-- It should have two functions:

-- logObject should take the class variable and a string for the file
-- it was generated from and produce a message

-- compareFromEntry should take a string and the class variable and assess
-- whether or not they are the same, returning a String message and a boolean.

-- Create instances of this class for the 'Person' and 'Point3' types.
-- Use the sample output to guide you
data Person = Person String String Int
  deriving (Show, Read, Eq)

-- logObject (Person "John" "Smith" 32) "Test.hs" -> "Produced 'Person \"John\" \"Smith\" 32' from file Test.hs"
-- compareFromEntry "Person \"John\" \"Smith\" 32" (Person "John" "Smith" 32)
--   -> (True, "Found entered Person object equivalent.")
-- compareFromEntry "Adult \"Jane\" \"Smith\" 32" (Adult "John" "Smith" 32)
--   -> (False, "Found entered Person object does not match.")

data Point3 = Point3 Int Int Int
  deriving (Show, Read, Eq)

-- logObject (Point3 3 4 5) "Test.hs" -> "Calculated 'Point 3 4 5' from input file Test.hs"
-- compareFromEntry (Point3 3 4 5) (Point3 3 4 5)
--   -> (True, "New Point calculation matches.")
-- compareFromEntry (Point3 3 4 5) (Point3 3 4 5)
--   -> (False, "New Point calculation does not match previous.")

-- Fill in this function as a generalization of compareFromEntry
-- Should return a Bool for whether or not the 'read' input matches the given
-- value. The output String should simply 'show' both values on either side of "vs."
--
-- compareAndPrint "Point3 3 4 5" (Point3 6 8 10)
--   -> (False, "'Point3 3 4 5' vs. 'Point3 6 8 10'")
compareAndPrint :: (Debuggable a) => String -> a -> (Bool, String)
compareAndPrint = undefined

-- Test Code

main :: IO ()
main = defaultMain $ testGroup "Typeclasses5" $
  [ testCase "Log Object 1" $ logObject (Person "John" "Smith" 32) "Test.hs" @?= "Produced 'Person \"John\" \"Smith\" 32' from file Test.hs"
  , testCase "Log Object 2" $ logObject (Person "Jane" "Doe" 31) "Input.txt" @?= "Produced 'Person \"Jane\" \"Doe\" 31' from file Input.txt"
  , testCase "Log Object 3" $ logObject (Point3 3 4 5) "Test.hs" @?= "Calculated 'Point3 3 4 5' from input file Test.hs"
  , testCase "Log Object 4" $ logObject (Point3 6 8 10) "Input.csv" @?= "Calculated 'Point3 6 8 10' from input file Input.csv"
  , testCase "Compare From Entry 1" $ compareFromEntry "Person \"John\" \"Smith\" 32" (Person "John" "Smith" 32) @?=
      (True, "Found entered Person object equivalent.")
  , testCase "Compare From Entry 2" $ compareFromEntry "Person \"John\" \"Smith\" 32" (Person "Jane" "Smith" 35) @?=
      (False, "Found entered Person object does not match.")
  , testCase "Compare From Entry 3" $ compareFromEntry "Point3 3 4 5" (Point3 3 4 5) @?=
      (True, "New Point calculation matches.")
  , testCase "Compare From Entry 4" $ compareFromEntry "Point3 3 4 5" (Point3 6 8 10) @?=
      (False, "New Point calculation does not match previous.")
  , testCase "Compare and Print 1" $ compareAndPrint "Person \"John\" \"Smith\" 32" (Person "John" "Smith" 32) @?=
      (True, "'Person \"John\" \"Smith\" 32' vs. 'Person \"John\" \"Smith\" 32'")
  , testCase "Compare and Print 2" $ compareAndPrint "Person \"John\" \"Smith\" 32" (Person "Jane" "Smith" 31) @?=
      (False, "'Person \"John\" \"Smith\" 32' vs. 'Person \"Jane\" \"Smith\" 31'")
  , testCase "Compare and Print 3" $ compareAndPrint "Point3 3 4 5" (Point3 3 4 5) @?=
      (True, "'Point3 3 4 5' vs. 'Point3 3 4 5'")
  , testCase "Compare and Print 4" $ compareAndPrint "Point3 3 4 5" (Point3 6 8 10) @?=
      (False, "'Point3 3 4 5' vs. 'Point3 6 8 10'")
  ]

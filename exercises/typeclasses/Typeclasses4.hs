-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- There will, of course, be times where we want to define our own typeclasses
  and common behaviors. We can define a typeclass with the 'class' keyword.
  We then continue with the name of our class then a lowercase variable name.
  Then, as with our instance, we use the 'where' keyword:

class Mathable a where
  ...

- Then you simply list the function names and type signatures for the
  different functions you want in that class! These will almost always
  contain the variable type:
  
-}

class Mathable a where
  getSum :: a -> Int
  getProduct :: a -> Int
  getMin :: a -> Int
  getMax :: a -> Int

-- Here's an instance for this class:
data Point3 = Point3 Int Int Int

instance Mathable Point3 where
  getSum (Point3 a b c) = a + b + c
  getSum (Point3 a b c) = a * b * c
  getMin (Point3 a b c) = min a (min b c)
  getMax (Point3 a b c) = max a (max b c)

-- TODO:

-- Make a class 'HasName'
-- This should have two functions that take the target type and return a String
-- 'getName' should return the object's full name.
-- 'greet' should return the string "Hello there, " and then append the full name.

-- Make an instance of this class for both 'Adult' and 'Child'
data Adult = Adult
  { firstName :: String
  , lastName :: String
  , adultAge :: Int
  }

data Child = Child
  { name :: String
  , childAge :: Int
  , grade :: Int
  }

-- Take two objects of potentially different types and give an ordering based
-- on their full name (i.e. using getName)
-- (Add constraints as necessary)
compareByName :: a -> b -> Ordering
compareByName = undefined

-- Test Code

main :: IO ()
main = defaultMain $ testGroup "Typeclasses4" $
  [ testCase "Get Name 1" $ getName (Adult "John" "Smith" 45) @?= "John Smith"
  , testCase "Get Name 2" $ getName (Child "Chris" 17 12) @?= "Chris"
  , testCase "Greet 1" $ greet (Adult "John" "Smith" 45) @?= "Hello there, John Smith"
  , testCase "Greet 2" $ greet (Child "Chris" 17 12) @?= "Hello there, Chris"
  , testCase "Compare 1" $ compareByName (Child "Chris" 17 12) (Adult "John" "Smith" 45) @?= LT
  , testCase "Compare 2" $ compareByName (Adult "Timothy" "Winston" 43) (Child "Edward" 15 10) @?= GT
  ]

-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- A 'typeclass' allows us to share common behavior between different types.
  The most basic example is the `Eq` class. This class allows us to determine
  if two members of a class are "equal" or not.

- Every class has associated functions. For `Eq`, we have these two operators:

(==) :: a -> a -> Bool

(/=) :: a -> a -> Bool

- We can use this class (and these operators) automatically with basic built-in types:

sumsEqual :: (Int, Int) -> (Int, Int) -> Bool
sumsEqual (a, b) (c, d) = a + b == c + d

-}

-- If a function requires us to compare inputs for equality,
-- we add a "constraint" on the Eq typeclass.
-- The input itself is a lowercase "variable" in the type signature and
-- we separate the constraint on that type using `=>`
equalMessages :: (Eq a) => a -> a -> String
equalMessages x y = if x == y
  then "Equal!"
  else "Not equal!"

-- The 'Show' class allows us to convert a class into a string by using
-- the 'show' function:
--
-- show :: a -> String
objectMessage :: (Show a) => a -> String
objectMessage obj = "Object is: " ++ show obj

-- If you make your own type, you must make it an 'instance' of the typeclass.
-- With simple classes like 'Eq' and 'Show', you can *derive* the instance.
-- To do this, use the 'deriving' keyword with the classes after you definition
--
-- data MyType = ...
--   deriving (Show, Eq)
--
-- This will create a "default" instance of the class, which is usually
-- reasonable enough.

-- TODO: Define instances of 'Show' and 'Eq' for these types and then fill in
--       'equalMessage' function so they tests can use it on the types.

data Occupation = Lawyer | Programmer | Engineer | Doctor | Manager | Teacher

data Person =
  Adult String String Int Occupation |
  Child String Int Int

-- A 'newtype' can also have typeclass instances!
-- (Basic type synonyms cannot)
newtype InterestRate = InterestRate Double

-- equalMessage should compare two objects of the same type.
-- If they are the same, it should return "Objects are both '{x}'!"
--   (where {x} is replaced by the "show" instance for the input object, keep the apostrophes)
-- If they are not equal, return "Objects '{x}' and '{y}' are not equal!"
-- Include a type signature!
equalMessage :: ???
equalMessage = undefined

-- Testing Code

main :: IO ()
main = defaultMain $ testGroup "Typeclasses1" $
  [ testCase "Occupation 1" $ equalMessage Lawyer Lawyer @?= "Objects are both 'Lawyer'!"
  , testCase "Occupation 2" $ equalMessage Programmer Engineer @?= "Objects 'Programmer' and 'Engineer' are not equal!"
  , testCase "Person 1" $ equalMessage (Adult "John" "Smith" 32 Doctor) (Adult "Jane" "Smith" 31 Engineer) @?=
      "Objects 'Adult \"John\" \"Smith\" 32 Doctor' and 'Adult \"Jane\" \"Smith\" 31 Engineer' are not equal!"
  , testCase "Person 2" $ equalMessage (Adult "John" "Smith" 32 Doctor) (Child "Chris" 12 7) @?=
      "Objects 'Adult \"John\" \"Smith\" 32 Doctor' and 'Child \"Chris\" 12 7' are not equal!"
  , testCase "Person 3" $ equalMessage (Child "Chris" 12 7) (Child "Chris" 12 7) @?=
      "Objects are both 'Child \"Chris\" 12 7'!"
  , testCase "Interest Rate 1" $ equalMessage (InterestRate 0.5) (InterestRate 0.5) @?= "Objects are both 'InterestRate 0.5'!"
  , testCase "Interest Rate 2" $ equalMessage (InterestRate 0.7) (InterestRate 0.5) @?=
      "Objects 'InterestRate 0.7' and 'InterestRate 0.5' are not equal!"
  ]

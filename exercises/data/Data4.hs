-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- Haskell allows us to *parameterize* our types. This means the type name contains
  one or more lowercase "variable" types:

data Grade letter = ...

- We can use this variable as a substitute for a normal type in a constructor:

data Grade letter = Grade Int letter String

- An important type that uses a parameter is 'Maybe'. This type wraps any other
  type 'a', and it has two constructors. The 'Just' constructor just wraps up
  the object, indicating a successful operation. The 'Nothing' constructor indicates
  we have nothing.

data Maybe a = Nothing | Just a

- This is useful for functions that can fail. For example, we can't take the square
  root of a negative number. Our program will crash if we try! So instead we handle
  it gracefully with `Maybe`:

safeSquareRoot :: Double -> Maybe Double
safeSquareRoot x = if x < 0 then Nothing else Just (sqrt x)

- The 'Either' type is very similar to 'Maybe' except that it has *two* parameters.
  In addition to the "success" type 'b', this has another type 'a' to represent the
  errors. This lets you customize failures, which you can't do with 'Maybe'.

data Either a b =
  Left a |
  Right b

- Of course, a list is also a parameterized type, since we can have a list of
  any underlying type. So you've been using these all along!

-}

data Occupation =
  Lawyer |
  Programmer |
  Engineer |
  StoreOwner |
  Fireman |
  PoliceOfficer |
  FlightAttendent

-- TODO: Change this type so that the last field (occupation) is *parameterized*.
--       Examples down below will variously use 'String' or the 'Occupation' type.
data Adult = Adult String String Int String

adult1 :: Adult String
adult1 = Adult "John" "Smith" 45 "Lawyer"

adult2 :: Adult Occupation
adult2 = Adult "Jane" "Smith" 39 Engineer

-- Return a tuple with the 2nd and 3rd elements of the list.
-- If there are fewer than 3 elements, return Nothing.
secondAndThird :: [Int] -> Maybe (Int, Int)
secondAndThird = undefined

-- Like above, but if there aren't enough elements, return a String saying:
-- "Only {x} element(s) in the list"
secondAndThird' :: [Int] -> Either String (Int, Int)
secondAndThird' = undefined

-- Testing Code
main :: IO ()
main = defaultMain $ testGroup "Syntax1" $
  [ testCase "secondAndThird 1" $ secondAndThird [] @?= Nothing
  , testCase "secondAndThird 2" $ secondAndThird [1] @?= Nothing
  , testCase "secondAndThird 3" $ secondAndThird [1, 2] @?= Nothing
  , testCase "secondAndThird 4" $ secondAndThird [1, 2, 3] @?= Just (2, 3)
  , testCase "secondAndThird 5" $ secondAndThird [1, 2, 3, 4] @?= Just (2, 3)
  , testCase "secondAndThird 6" $ secondAndThird [5, 3, 1, 4, 5, 6] @?= Just (3, 1)
  , testCase "secondAndThird' 1" $ secondAndThird' [] @?= Left "Only 0 element(s) in the list"
  , testCase "secondAndThird' 2" $ secondAndThird' [1] @?= Left "Only 1 element(s) in the list"
  , testCase "secondAndThird' 3" $ secondAndThird' [1, 2] @?= Left "Only 2 element(s) in the list"
  , testCase "secondAndThird' 4" $ secondAndThird' [1, 2, 3] @?= Right (2, 3)
  , testCase "secondAndThird' 5" $ secondAndThird' [1, 2, 3, 4] @?= Right (2, 3)
  , testCase "secondAndThird' 6" $ secondAndThird' [5, 3, 1, 4, 5, 6] @?= Right (3, 1)
  ]

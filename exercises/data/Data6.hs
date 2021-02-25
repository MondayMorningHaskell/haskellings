-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- Type synonyms are nice, but they have a weakness in that they don't affect
  the compile-time behavior of our program at all. Consider our login example:

type Username = String
type Password = String

myUsername :: Username
myUsername = "user1234"

password :: Password
password = "password1234"

login :: Username -> Password -> LoginResult

result = login password myUsername

- In this case, we've mixed up the order of our inputs, but this code will still
  compile! We'll only catch the error at runtime. In Haskell we prefer to catch
  issues at compile-time whenever possible. In this case the solution is to use
  a *newtype* instead of a type synonym.

- A *newtype* is like defining a new data type, except it only has a single
  constructor with a single type attached to it. So it just wraps one object.

newtype Username = Username String
newtype Password = Password String

- Now when we construct a newtyped item, we have to use the constructor. If we
  pass mismatching newtyped objects, we'll now get a compile error!

myUsername :: Username
myUsername = Username "user1234"

password :: Password
password = Password "password1234"

-- Throws a compile error!
result = login password myUsername

- Note a newtype wrapper is more efficient than using *data* to wrap the type.
  At runtime (after compilation), Haskell will treat the object like its
  underlying type, so it doesn't need to spend time unwrapping.

- A newtype can also use record syntax:

newtype Username = Username { unUsername :: String }

-}

-- TODO: There are a few inaccuracies in this code, even though it compiles! 
--       Turn these type synonyms into newtypes so they cause compilation errors.
--       Then fix the errors!

type Slope = Double
type Intercept = Double
type XCoordinate = Double
type YCoordinate = Double

-- NOTE: Add this line after the 'YCoordinate' newtype declaration:
--
--  -->  deriving (Show, Eq)
--
--       We'll see what it means soon!

x1 :: XCoordinate
x1 = 4.0

s1 :: Slope
s1 = -3.0

i1 :: Intercept
i1 = 2.0

y1 :: YCoordinate
y1 = calculateY x1 i1 s1

expectedY1 :: YCoordinate
expectedY1 = -10.0

calculateY :: Slope -> Intercept -> XCoordinate -> YCoordinate
calculateY intercept slope x = slope * x + intercept

-- Change these to account for newtypes!
x2 :: XCoordinate
x2 = 3.5

s2 :: Slope
s2 = 2.3

i2 :: Intercept
i2 = 3.7

y2 :: YCoordinate
y2 = 11.75

-- Testing Code

main :: IO ()
main = defaultMain $ testGroup "Data6" $
  [ testCase "Slope 1" $ calculateY s1 i1 x1 @?= expectedY1
  , testCase "Slope 2" $ calculateY s2 i2 x2 @?= y2
  ]

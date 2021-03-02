-- I AM NOT DONE

import Data.Monoid
import Data.Semigroup

import Test.Tasty
import Test.Tasty.HUnit

{-

- Monoids and Semigroups are two more types of functional structures.
  They aren't as connected to Monads as Functors and Applicatives, but
  we'll still need them.

- A Semigroup is any type that is "appendable". That is, we can take
  two elements of the type and combine them into a new element within the type.
  This behavior is captured by the append operator (<>):

class Semigroup a where
  (<>) :: a -> a -> a

- The clearest example of a semigroup is, of course, a list. The 'append'
  function is simply the list append operator (++) we've been using:

instance Semigroup [a] where
  (<>) = (++)

- A 'Monoid' is an extension of a Semigroup. What defines a Monoid is that
  there is a characteristic "Identity" element called 'mempty'. If this
  object is appended on either side of another element 'a', then the result
  is still equivalent to 'a'. For a list, this element is clearly '[]', the empty list.

class (Semigroup a) => Monoid a where
  mempty :: a

instance Monoid [a] where
  mempty = []

-}

-- TODO:

-- Write Semigroup and Monoid instances for these two types.
-- In the first, "appending" should work like addition.
-- In the second, "appending" should work like multiplication.

newtype IntAdd = IntAdd Int

newtype IntMultiply = IntMultiply Int

-- Write a function that takes any two items of an "appendable" type.
-- The result should be the two items appended in an "ABBA" pattern.
-- abba [1, 2] [3] = [1, 2, 3, 3, 1, 2]
-- abba (IntMultiply 4) (IntMultiply 6) = IntMultiply (4 * 6 * 6 * 4) -- 576
-- Write the type signature yourself.
abba :: ???
abba = undefined

main :: IO ()
main = defaultMain $ testGroup "Monoids" $
  [ testCase "IntAdd 1" $ (IntAdd 5) <> (IntAdd 6) @?= (IntAdd 11)
  , testCase "IntAdd 2" $ (IntAdd 3) <> (IntAdd (-11)) @?= (IntAdd (-8))
  , testCase "IntAdd 3" $ (mempty :: IntAdd) @?= (IntAdd 0)
  , testCase "IntMultiply 1" $ (IntMultiply 5) <> (IntMultiply 6) @?= (IntMultiply 30)
  , testCase "IntMultiply 2" $ (IntMultiply 3) <> (IntMultiply (-11)) @?= (IntMultiply (-33))
  , testCase "IntMultiply 3" $ (mempty :: IntMultiply) @?= (IntMultiply 1)
  , testCase "abba 1" $ abba (IntAdd 5) (IntAdd 6) @?= (IntAdd 22)
  , testCase "abba 2" $ abba (IntMultiply 5) (IntMultiply 6) @?= (IntMultiply 900)
  , testCase "abba 3" $ abba [1, 2, 3] [4, 5, 6] @?= [1, 2, 3, 4, 5, 6, 4, 5, 6, 1, 2, 3]
  , testCase "abba 4" $ abba "Hello " "Goodbye " @?= "Hello Goodbye Goodbye Hello "
  ]

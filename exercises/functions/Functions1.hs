
import Test.Tasty
import Test.Tasty.HUnit

{-

- This is our first function. Functions are special expressions
  that require one or more "inputs".

- In our type signature (Int -> Int), the first type listed is the
  "input" type. The second type (after the arrow) indicates the "result" type.
  (We can also call it the "return" type)

- We assign a name ("x" in this case) to the input and put it on the left side
  of the expression definition.

-}

add5 :: Int -> Int
add5 x = x + 5

{-

- We "apply" a function by simply placing the input argument after the function name.

- Unlike other languages, we don't use parentheses when calling functions.
  In Python/Java/Javascript/C++, this might look like "eleven = add5(6)"

-}

eleven :: Int
eleven = add5 6

{-

- Functions can have more than one argument. We simply have more types and arrows
  before our final return value, and more arguments before "="

-}

multiplyAndAdd6 :: Int -> Int -> Int
multiplyAndAdd6 x y = x * y + 6

-- TODO: Fill in these functions and expressions, including the type signatures
subtract7 :: Int -> Int
subtract7 x = x - 7

-- Get this by using "subtract7"
twelve :: Int
twelve = subtract7 19

-- Take 2 inputs. Multiply them together, and then multiply by 5.
multiplyProductBy5 :: Int -> Int -> Int
multiplyProductBy5 x y = x * y * 5

-- Find some combination of arguments to multiplyProductBy5 that produces 60.
sixty :: Int
sixty = multiplyProductBy5 3 4

-- Testing Code, You can ignore this:
main :: IO ()
main = defaultMain $ testGroup "Functions1" $
  [ testCase "Subtract 1" $ subtract7 13 @?= 6
  , testCase "Subtract 2" $ subtract7 27 @?= 20
  , testCase "Twelve" $ twelve @?= 12
  , testCase "MultiplyProductBy5 1" $ multiplyProductBy5 5 5 @?= 125
  , testCase "MultiplyProductBy5 2" $ multiplyProductBy5 1 10 @?= 50
  , testCase "MultiplyProductBy5 3" $ multiplyProductBy5 2 22 @?= 220
  , testCase "Sixty" $ sixty @?= 60
  ]

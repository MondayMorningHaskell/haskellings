-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- Monads are extremely useful for combining different operations together within
  the same context. But using only the bind operator is limiting. It makes it
  difficult to pass several different computations.

- The solution to this is *do-syntax*. Under this syntax, we can list several different
  computations within our monad and freely use the results of all previous computations.

- Suppose, in the 'Maybe' monad we want to combine the output from two different
  operations into a final result.

op1 :: String -> Maybe String
op2 :: String -> Maybe String
op3 :: String -> String -> Maybe String

- Here's how we can do this using do-syntax:

finalOp :: String -> Maybe String
finalOp input = do
  output1 <- op1 input
  output2 <- op2 input
  op3 output1 output2

- On each line of do-syntax, we use the '<-' operator, which unwraps our
  value out of the context. On the *Right* side of the operator we have
  the original monadic operation. The final type should be 'Maybe x' here.
  On the *Left* side of the operator we have the unwrapped value. It should
  typically *not* be a 'Maybe' value.

- Then the *last* line of the block should simply be a Monadic value ('Maybe')

- Here's the code again, with type annotations:

finalOp :: String -> Maybe String
finalOp input = do
-- Left: Unwrapped result    Right: Wrapped computation
  (output1 :: String)     <- (op1 input :: Maybe String)
  (output2 :: String)     <- (op2 input :: Maybe String)
  -- Final line: Wrapped computation
  (op3 output1 output2 :: Maybe String)

- The key is that do-syntax understands how these operations combine. If
  'op1' returns 'Nothing', the whole computation immediately returns 'Nothing'.
  It never needs to see the unwrapped 'output1' value.

- We could do the same thing, except with the List monad!

op1 :: String -> [String]
op2 :: String -> [String]
op3 :: String -> String -> [String]

finalOp :: String -> [String]
finalOp input = do
-- Left: Unwrapped result    Right: Wrapped computation
  (output1 :: String)     <- (op1 input :: [String])
  (output2 :: String)     <- (op2 input :: [String])
  -- Final line: Wrapped computation
  (op3 output1 output2 :: [String])

- Each time it "unwraps" a String, like 'output1', we can imagine that this
  represents every individual result of 'op1'. So 'op2' will get called on
  each result of 'op1', and so on.

-}

safeSquareRoot :: Double -> Maybe Double
safeSquareRoot x = if x < 0 then Nothing else Just (sqrt x)

multiplyIfSmall :: Double -> Double -> Maybe Double
multiplyIfSmall y x = if x < 9.5 then Just (y * x) else Nothing

-- TODO:

-- In the last couple exercises, you've implemented these functions either
-- using the applicative operator <*> or the monadic bind operator >>=.
-- Re-implement them all now using do-syntax!

sumOfSquareRoots :: Double -> Double -> Maybe Double
sumOfSquareRoots = ???

generateAllResults :: [Int -> Int -> Int] -> [Int] -> [Int] -> [Int]
generateAllResults = ???

sqrtAndMultiply :: Double -> Maybe Double
sqrtAndMultiply = ???

addAndNegate :: [Int] -> [Int]
addAndNegate = ???

main :: IO ()
main = defaultMain $ testGroup "Monads1" $
  [ testCase "sumOfSquareRoots 1" $ sumOfSquareRoots 4.0 9.0 @?= Just 5.0
  , testCase "sumOfSquareRoots 2" $ sumOfSquareRoots (-4.0) 9.0 @?= Nothing
  , testCase "sumOfSquareRoots 3" $ sumOfSquareRoots (-4.0) (-9.0) @?= Nothing
  , testCase "sumOfSquareRoots 4" $ sumOfSquareRoots 4.0 (-9.0) @?= Nothing
  , testCase "sumOfSquareRoots 5" $ sumOfSquareRoots 4.0 0.0 @?= Just 2.0
  , testCase "generateAllResults 1" $ generateAllResults ([] :: [Int -> Int -> Int]) [1, 2] [3, 4] @?= []
  , testCase "generateAllResults 2" $ generateAllResults [(+) :: Int -> Int -> Int, (*)] [] [3, 4] @?= []
  , testCase "generateAllResults 3" $ generateAllResults [(+)] [1, 2] [3, 4] @?= [4, 5, 5, 6]
  , testCase "generateAllResults 4" $ generateAllResults [(+), (*)] [1, 2] [3, 4] @?= [4, 5, 5, 6, 3, 4, 6, 8]
  , testCase "generateAllResults 5" $ generateAllResults [(+), (*), (-)] [10] [3, 4, 5] @?= [13, 14, 15, 30, 40, 50, 7, 6, 5]
  , testCase "sqrtAndMultiply 1" $ sqrtAndMultiply 0.0 @?= Just 0.0
  , testCase "sqrtAndMultiply 2" $ sqrtAndMultiply 4.0 @?= Just 20.0
  , testCase "sqrtAndMultiply 3" $ sqrtAndMultiply 81.0 @?= Just 90.0
  , testCase "sqrtAndMultiply 4" $ sqrtAndMultiply 121.0 @?= Nothing
  , testCase "sqrtAndMultiply 5" $ sqrtAndMultiply 144.0 @?= Nothing
  , testCase "sqrtAndMultiply 6" $ sqrtAndMultiply (-4.0) @?= Nothing
  , testCase "addAndNegate 1" $ addAndNegate [] @?= []
  , testCase "addAndNegate 2" $ addAndNegate [10] @?= [11, -11, 12, -12, 13, -13]
  , testCase "addAndNegate 3" $ addAndNegate [1, 2] @?= [2, -2, 3, -3, 4, -4, 3, -3, 4, -4, 5, -5]
  ]

-- I AM NOT DONE

import Control.Monad.Writer
import Test.Tasty
import Test.Tasty.HUnit

{-

- The *Writer* monad is the next step beyond the Reader monad. It gives us access
  to a "Write-Only" state that we can access when our computation is complete.
  The key is that the state we pass must be a 'Monoid'. So we can only change it
  by "appending" a new value.

instance (Monoid w) => Monad (Writer w) where
  ...

- The primary function you'll use within the Writer monad is 'tell'. Tell takes
  a value of the Monoid type and appends it to the existing state. So if you're
  tracking logs with your Writer, this would be where you append a log message.

tell :: w -> Writer w ()

- Just as Reader has the 'runReader' function, there is also the 'runWriter'
  function, which allows you to run writer computations.

runWriter :: Writer w a -> (a, w)

- The 'runReader' function requires an initial state. But the state is unchanged,
  so the only output is the computation result.

- We can see 'runWriter' is the opposite. There is no initial state for 'w'. It is
  assumed to be the 'mempty' of the Monoid instance. But the function produces both
  the computation values AND the accumulated monoid value.

-}

newtype IntAdd = IntAdd Int
  deriving (Show, Eq)

instance Semigroup IntAdd where
  (IntAdd x) <> (IntAdd y) = IntAdd (x + y)

instance Monoid IntAdd where
  mempty = IntAdd 0

-- Here we have several operations we might apply on an input Double.
-- Each of them has a particular cost associated with them.
data Op = Add Double | Subtract Double | Multiply Double | Divide Double | Sqrt

opCost :: Op -> IntAdd
opCost (Add _) = IntAdd 1
opCost (Subtract _) = IntAdd 2
opCost (Multiply _) = IntAdd 5
opCost (Divide _) = IntAdd 10
opCost Sqrt = IntAdd 20

instance Show Op where
  show (Add x) = "Adding " ++ show x
  show (Subtract x) = "Subtracting " ++ show x
  show (Multiply x) = "Multiplying by " ++ show x
  show (Divide x) = "Dividing by " ++ show x
  show Sqrt = "Taking Square Root"

-- TODO:

-- Writer a function that will return the result of the operation AND add
-- its cost to the monad (using 'opCost' above).
-- (If the input to 'Sqrt' is negative, just return the original value)
applyOpCount :: Op -> Double -> Writer IntAdd Double
applyOpCount = ???

-- Now apply a series of operations to an input value using the 'Writer' monad!
applyAndCountOperations :: [Op] -> Double -> (Double, IntAdd)
applyAndCountOperations = ???

-- Do the same as above, except instead of counting the cost, log the
-- string associated with 'showing' the operation.
applyOpLog :: Op -> Double -> Writer [String] Double
applyOpLog = ???

applyAndLogOperations :: [Op] -> Double -> (Double, [String])
applyAndLogOperations = ???

-- Test Code

opList1 :: [Op]
opList1 = [Add 5.0, Subtract 2.0, Multiply 3.0, Divide 2.0]

opList2 :: [Op]
opList2 = [Multiply 2.0, Sqrt]

main :: IO ()
main = defaultMain $ testGroup "Monads4" $
  [ testCase "applyOpCount 1" $ runWriter (applyOpCount (Add 1) 5.0) @?= (6.0, IntAdd 1)
  , testCase "applyOpCount 2" $ runWriter (applyOpCount (Multiply 2) 5.0) @?= (10.0, IntAdd 5)
  , testCase "applyOpCount 3" $ runWriter (applyOpCount (Divide 2) 6.0) @?= (3.0, IntAdd 10)
  , testCase "applyOpCount 4" $ runWriter (applyOpCount Sqrt 4.0) @?= (2.0, IntAdd 20)
  , testCase "applyAndCountOperations 1" $ applyAndCountOperations [] 5.0 @?= (5.0, IntAdd 0)
  , testCase "applyAndCountOperations 2" $ applyAndCountOperations [Add 1.0] 5.0 @?= (6.0, IntAdd 1)
  , testCase "applyAndCountOperations 3" $ applyAndCountOperations [Subtract 3.0] 8.0 @?= (5.0, IntAdd 2)
  , testCase "applyAndCountOperations 4" $ applyAndCountOperations opList1 13.0 @?= (24.0, IntAdd 18)
  , testCase "applyAndCountOperations 5" $ applyAndCountOperations opList2 8.0 @?= (4.0, IntAdd 25)
  , testCase "applyOpLog 1" $ runWriter (applyOpLog (Add 1) 5.0) @?= (6.0, ["Adding 1.0"])
  , testCase "applyOpLog 2" $ runWriter (applyOpLog (Multiply 2) 5.0) @?= (10.0, ["Multiplying by 2.0"])
  , testCase "applyOpLog 3" $ runWriter (applyOpLog (Divide 2) 6.0) @?= (3.0, ["Dividing by 2.0"])
  , testCase "applyOpLog 4" $ runWriter (applyOpLog Sqrt 4.0) @?= (2.0, ["Taking Square Root"])
  , testCase "applyAndLogOperations 1" $ applyAndLogOperations [] 5.0 @?= (5.0, [])
  , testCase "applyAndLogOperations 2" $ applyAndLogOperations [Add 1.0] 5.0 @?= (6.0, ["Adding 1.0"])
  , testCase "applyAndLogOperations 3" $ applyAndLogOperations [Subtract 3.0] 8.0 @?= (5.0, ["Subtracting 3.0"])
  , testCase "applyAndLogOperations 4" $ applyAndLogOperations opList1 13.0 @?=
      (24.0, ["Adding 5.0", "Subtracting 2.0", "Multiplying by 3.0", "Dividing by 2.0"])
  , testCase "applyAndLogOperations 5" $ applyAndLogOperations opList2 8.0 @?=
      (4.0, ["Multiplying by 2.0", "Taking Square Root"])
  ]

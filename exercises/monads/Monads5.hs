-- I AM NOT DONE

import Control.Monad.State
import Test.Tasty
import Test.Tasty.HUnit

{-

- The *State* monad combines the functionality of the Reader and Writer monads.
  We have a single stateful object, and we are free to access and read from it,
  or update and change its values. When we change the object, subsequent operations
  in the monad will refer to the updated value.

instance Monad (State s) where
  ...

- Note the state does NOT have to be a Monoid, as with Writer

- We retrieve the state with the 'get' function. We can replace the state with
  a new object by using 'put':

get :: State s s
put :: s -> State s ()

- The 'runState' function works like 'runReader' and 'runWriter'. It requires
  an initial state parameter and produces both the computation result and the
  final state.

runState :: State s a -> s -> (a, s)

- If you only care about the final computation result, you can use
  'evalState' instead of 'runState'. If you only care about the final
  state, you can use 'execState':

evalState :: State s a -> s -> a
execState :: State s a -> s -> s

- There are a couple other functions you can use. Just like we have
  'asks' in Reader, there is 'gets' which can retrieve a field from the State.

gets :: (s -> a) -> State s a

- Then you can use 'modify' to apply a function on the state:

modify :: (s -> s) -> State s ()

runState (modify (+4) 5) -> ((), 9)

-}

newtype IntAdd = IntAdd Int
  deriving (Show, Eq)

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

-- Rewrite these functions from last time, but use the State monad
-- instead of the Writer monad! Notice however, that you do not
-- need a 'Monoid' instance for the "IntAdd" state!
applyOpCount :: Op -> Double -> State IntAdd Double
applyOpCount = ???

applyAndCountOperations :: [Op] -> Double -> (Double, IntAdd)
applyAndCountOperations = ???

applyOpLog :: Op -> Double -> State [String] Double
applyOpLog = ???

applyAndLogOperations :: [Op] -> Double -> (Double, [String])
applyAndLogOperations = ???

-- Now write these in a simpler way, where the 'State' is the
-- Double itself that you are tracking with the operations.
-- This spares you from needing to track it as a separate input!
applyOpSimple :: Op -> State Double ()
applyOpSimple = ???

applySimpleOperations :: [Op] -> Double -> Double
applySimpleOperations = ???

-- Test Code

opList1 :: [Op]
opList1 = [Add 5.0, Subtract 2.0, Multiply 3.0, Divide 2.0]

opList2 :: [Op]
opList2 = [Multiply 2.0, Sqrt]

main :: IO ()
main = defaultMain $ testGroup "Monads5" $
  [ testCase "applyOpCount 1" $ runState (applyOpCount (Add 1) 5.0) (IntAdd 0) @?= (6.0, IntAdd 1)
  , testCase "applyOpCount 2" $ runState (applyOpCount (Multiply 2) 5.0) (IntAdd 0) @?= (10.0, IntAdd 5)
  , testCase "applyOpCount 3" $ runState (applyOpCount (Divide 2) 6.0) (IntAdd 0) @?= (3.0, IntAdd 10)
  , testCase "applyOpCount 4" $ runState (applyOpCount Sqrt 4.0) (IntAdd 0) @?= (2.0, IntAdd 20)
  , testCase "applyAndCountOperations 1" $ applyAndCountOperations [] 5.0 @?= (5.0, IntAdd 0)
  , testCase "applyAndCountOperations 2" $ applyAndCountOperations [Add 1.0] 5.0 @?= (6.0, IntAdd 1)
  , testCase "applyAndCountOperations 3" $ applyAndCountOperations [Subtract 3.0] 8.0 @?= (5.0, IntAdd 2)
  , testCase "applyAndCountOperations 4" $ applyAndCountOperations opList1 13.0 @?= (24.0, IntAdd 18)
  , testCase "applyAndCountOperations 5" $ applyAndCountOperations opList2 8.0 @?= (4.0, IntAdd 25)
  , testCase "applyOpLog 1" $ runState (applyOpLog (Add 1) 5.0) [] @?= (6.0, ["Adding 1.0"])
  , testCase "applyOpLog 2" $ runState (applyOpLog (Multiply 2) 5.0) [] @?= (10.0, ["Multiplying by 2.0"])
  , testCase "applyOpLog 3" $ runState (applyOpLog (Divide 2) 6.0) [] @?= (3.0, ["Dividing by 2.0"])
  , testCase "applyOpLog 4" $ runState (applyOpLog Sqrt 4.0) [] @?= (2.0, ["Taking Square Root"])
  , testCase "applyAndLogOperations 1" $ applyAndLogOperations [] 5.0 @?= (5.0, [])
  , testCase "applyAndLogOperations 2" $ applyAndLogOperations [Add 1.0] 5.0 @?= (6.0, ["Adding 1.0"])
  , testCase "applyAndLogOperations 3" $ applyAndLogOperations [Subtract 3.0] 8.0 @?= (5.0, ["Subtracting 3.0"])
  , testCase "applyAndLogOperations 4" $ applyAndLogOperations opList1 13.0 @?=
      (24.0, ["Adding 5.0", "Subtracting 2.0", "Multiplying by 3.0", "Dividing by 2.0"])
  , testCase "applyAndLogOperations 5" $ applyAndLogOperations opList2 8.0 @?=
      (4.0, ["Multiplying by 2.0", "Taking Square Root"])
  , testCase "applyOpSimple 1" $ execState (applyOpSimple (Add 1)) 5.0 @?= 6.0
  , testCase "applyOpSimple 2" $ execState (applyOpSimple (Multiply 2)) 5.0 @?= 10.0
  , testCase "applyOpSimple 3" $ execState (applyOpSimple (Divide 2)) 6.0 @?= 3.0
  , testCase "applyOpSimple 4" $ execState (applyOpSimple Sqrt) 4.0 @?= 2.0
  , testCase "applySimpleOperations 1" $ applySimpleOperations [] 5.0 @?= 5.0
  , testCase "applySimpleOperations 2" $ applySimpleOperations [Add 1.0] 5.0 @?= 6.0
  , testCase "applySimpleOperations 3" $ applySimpleOperations [Subtract 3.0] 8.0 @?= 5.0
  , testCase "applySimpleOperations 4" $ applySimpleOperations opList1 13.0 @?= 24.0
  , testCase "applySimpleOperations 5" $ applySimpleOperations opList2 8.0 @?= 4.0
  ]

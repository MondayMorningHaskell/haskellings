import Control.Monad.Trans.Class
import Control.Monad.State

{-

- Monad *Transformers* allow you to combine the effects of multiple monads.
  These transformers usually consist in taking a normal monad name and
  adding the letter 'T' to the end. Each transformer then has an extra
  parameter for an "underlying" monad. For example, there's 'StateT':

newtype StateT s m a = ...

instance (Monad m) => Monad (StateT s m) where
  ...

- All the same stateful functions we had before still work:

get :: StateT s m s
put :: s -> StateT s m ()
...

- Transformers have 'run' functions like normal monads. However, instead
  of returning a pure result, they return an action in the underlying monad:

runStateT :: StateT s m a -> s -> m (a, s)

- As an example to illustrate this, let's consider using 'IO' as our underlying monad:

doubleReturnTriple :: StateT Double IO Double
doubleReturnTriple = do
  x <- get
  put (2 * x)
  return (3 * x)

stateRunner :: IO (Double, Double)
stateRunner = do
  input <- read <$> getLine
  (result, finalState) <- runStateT doubleReturnTriple 3.0
  print result
  print finalState
  return (result, finalState) -- (9.0, 6.0)

- Using the 'lift' function, you can run a function in the "underlying" monad.
  It works in all transformer cases, but here's what the type signature looks
  like for our case:

lift :: IO a -> StateT s IO a

doubleReturnTriple :: StateT Double IO Double
doubleReturnTriple = do
  x <- get
  lift $ putStr "Starting value: "
  lift $ print x
  lift $ putStrLn "Doubling state!"
  put (2 * x)
  return (3 * x)

-}

data Op = Add Double | Subtract Double | Multiply Double | Divide Double | Sqrt
  deriving (Read)

opCost :: Op -> Int
opCost (Add _) = 1
opCost (Subtract _) = 2
opCost (Multiply _) = 5
opCost (Divide _) = 10
opCost Sqrt = 20

instance Show Op where
  show (Add x) = "Adding " ++ show x
  show (Subtract x) = "Subtracting " ++ show x
  show (Multiply x) = "Multiplying by " ++ show x
  show (Divide x) = "Dividing by " ++ show x
  show Sqrt = "Taking Square Root"

applyOp :: Op -> Double -> Double
applyOp (Add x) y = x + y
applyOp (Subtract x) y = y - x
applyOp (Multiply x) y = x * y
applyOp (Divide x) y = y / x
applyOp Sqrt y = if y < 0 then y else sqrt y

-- TODO:

-- Replicate the logic of applyAndCountOperations from the Monads4 exercise.
-- Store the "cost" of the operation in the 'State' value, and update the
-- final 'Double' value.
-- However, you should also print each operation as it is processed.
applyOperations :: [Op] -> Double -> StateT Int IO Double
applyOperations list d = f list d
  where 
    f :: [Op] -> Double -> StateT Int IO Double
    f [] d = return d
    f (op:ops) d = do
       lift $ putStrLn $ show op
       d' <- applyOpCount op d
       f ops d'

    applyOpCount :: Op -> Double -> StateT Int IO Double
    applyOpCount op d = do
      s <- get
      let cost = opCost op
      put $ cost + s
      return $ apply op d
  
    apply :: Op -> Double -> Double
    apply (Add x) d = d + x
    apply (Subtract x) d = d - x
    apply (Multiply x) d = d * x 
    apply (Divide x) d = d / x
    apply Sqrt _ = if d < 0 then d else sqrt d


-- Prompt the user for a number and three operations:
-- 'Please enter a number.'
-- 'Please enter three operations.'
-- Pass these inputs to 'applyOperations'.
-- Print the final tuple result from runState (e.g. 'Result: (1.0, 5)' )
main :: IO ()
main = do
  putStrLn "Please enter a number."
  num <- getLine
  let double = (read num :: Double)
  putStrLn "Please enter three operations."
  (state, result) <- run 0 double 0
  putStrLn $ "Result: (" ++ show state ++ "," ++ show result ++ ")"

  where
    run :: Int -> Double -> Int -> IO (Double, Int)
    run state double counter = do
      case counter < 3 of 
        True -> do 
          s <- getLine
          let op = (read s :: Op)
          (double, state) <- runStateT (applyOperations [op] double) state
          run state double (counter + 1)
        False -> do
          runStateT (applyOperations [] double) state
{-

Sample Input:

2.0
Add 4.0
Multiply 6.0
Sqrt

Sample Output:

Please enter a number.
Please enter three operations.
Adding 4.0
Multiplying by 6.0
Taking Square Root
Result: (6.0, 26)

-}

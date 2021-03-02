-- I AM NOT DONE

import Test.Tasty
import Test.Tasty.HUnit

{-

- Understanding *Monads* is a critical piece of learning Haskell. But before we
  can understand monads, we must first understand *Functors*.

- The *Functor* typeclass encapsulates the behavior of a type that
  contains another type and permits transformations over that type.
  It has one function: 'fmap'.

class Functor f where
  fmap :: (a -> b) -> f a -> f b

- This function transforms all of the underlying elements, but leaves
  the structure of the container intact.

- This should sound familiar, because this is exactly what the
  'map' function does for lists! Lists in fact, are a functor, and
  they implement the 'fmap' function by using `map`!

instance Functor [] where
  fmap = map

fmap (+2) [1, 2, 4] -> [3, 4, 6]

- The 'Maybe' and 'Either' types are also functors! They allow us to
  apply a function when we don't know if the underlying value was
  successful or not. If the original value was 'Nothing' or 'Left',
  nothing happens.

fmap (+2) (Just 4) -> Just 6
fmap (+2) Nothing -> Nothing
fmap (+2) (Right 4) -> Right 6
fmap (+2) (Left "Failed") -> Left "Failed"

- Instead of writing out 'fmap', you can also use the (<$>) operator:

(+2) <$> [1, 2, 4] -> [3, 4, 6]
(+2) <$> (Just 4) -> Just 6
(+2) <$> Nothing -> Nothing
(+2) <$> (Right 4) -> Right 6
(+2) <$> (Left "Failed") -> Left "Failed"

-}

safeSquareRoot :: Double -> Maybe Double
safeSquareRoot x = if x < 0 then Nothing else Just (sqrt x)

-- TODO:

-- Rewrite this function so that it uses 'fmap' instead of a case statement!
-- Try to get the definition on one, short line!
multiplySqrtDouble :: Double -> Double -> Maybe Double
multiplySqrtDouble x y = case safeSquareRoot product of
  Nothing -> Nothing
  Just z -> Just (z * 2)
  where
    product = x * y

-- Write a functor instance for this data type!

data Metrics m = Metrics
  { latestMeasurements :: [m]
  , average :: m
  , max :: m
  , min :: m
  , mode :: Maybe m
  } deriving (Show, Eq)

-- Now write a simple function with fmap to double the value of all the metrics!
doubleMetrics :: Metrics Double -> Metrics Double
doubleMetrics = undefined

-- 

m1 :: Metrics Double
m1 = Metrics [3.0, 6.0] 4.5 6.0 3.0 Nothing

m2 :: Metrics Double
m2 = Metrics [1.0, 1.0, 5.0, 0.5] 1.875 5.0 0.5 (Just 1.0)

main :: IO ()
main = defaultMain $ testGroup "Functors" $
  [ testCase "multiplySqrtDouble 1" $ multiplySqrtDouble (-2.0) (-8.0) @?= Just 8.0
  , testCase "multiplySqrtDouble 2" $ multiplySqrtDouble (-2.0) (8.0) @?= Nothing
  , testCase "multiplySqrtDouble 3" $ multiplySqrtDouble (3.0) (3.0) @?= Just 6.0
  , testCase "multiplySqrtDouble 4" $ multiplySqrtDouble (-3.0) 0 @?= Just 0.0
  , testCase "Metrics Functor 1" $ (*3) <$> m1 @?= Metrics [9.0, 18.0] 13.5 18.0 9.0 Nothing
  , testCase "Metrics Functor 2" $ (*3) <$> m2 @?= Metrics [3.0, 3.0, 15.0, 1.5] 5.625 15.0 1.5 (Just 3.0)
  , testCase "Double Metrics 1" $ doubleMetrics m1 @?= Metrics [6.0, 12.0] 9.0 12.0 6.0 Nothing
  , testCase "Double Metrics 2" $ doubleMetrics m2 @?= Metrics [2.0, 2.0, 10.0, 1.0] 3.75 10.0 1.0 (Just 2.0)
  ]

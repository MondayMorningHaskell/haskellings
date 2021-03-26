module Data5 where

-- I AM NOT DONE

{-

- Haskell allows us to create a *type synonym* using the "type" keyword.
  The synonym must also be capitalized, and we just "assign" it to the
  existing type. As we've already seen, the most common usage of this
  is that `String` is simply a list of characters:

type String = [Char]

- This can serve a few different purposes. For example, we can rename a
  commonly used type that is long to type out:

type IntTuple = (Int, Int, Int, Int)

addTuples :: IntTuple -> IntTuple -> IntTuple
...

- It can also allow us to draw a semantic difference between two items that
  appear to have the same "type", which can help guide the user and avoid
  unnecessary mistakes. Consider this type signature:

data LoginResult = Success | Failure

login :: String -> String -> LoginResult
...

- It's not 100% clear which strings we should enter.
  A password? An API Key? An email? A username?
  We can make this clearer like so;
  
type Username = String
type Password = String

login :: Username -> Password -> LoginResult
...

- Now someone reading our documentation will know what inputs they need.

-}

-- TODO: Add a few type synonyms to make these functions compile:

calculateMonthlyInterest :: Principal -> Rate -> Double
calculateMonthlyInterest p r = (p * r) / 12.0

calculateY :: Slope -> Intercept -> XCoordinate -> YCoordinate
calculateY slope intercept x = slope * x + intercept

greet :: Name -> Occupation -> String
greet n o = "Hello, my name is " ++ n ++ ". I am a " ++ o ++ "."

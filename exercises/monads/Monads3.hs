-- I AM NOT DONE

import Control.Monad.Reader
import Test.Tasty
import Test.Tasty.HUnit

{-

- Now that we know what monads are at a basic level and how they function,
  let's learn about some other common monads.

- The *Reader* Monad represents a computation context where we have a
  global variable that we can share our different functions. We can't
  assign to a globally accessible value the way we might in other
  languages. So this monad lets us share a particular value more easily.
  It is parameterized by the type of the global variable.

instance Monad (Reader r) where
  ...

- The simplest function you call within this monad is 'ask'.
  This retrieves the Reader state:

ask :: Reader r r

- Here's what a do-syntax function within 'Reader' might look like.
  Since the final expression must be within the 'Reader' monad, we
  use 'return' on the last line to wrap our answer:

add2AndShow :: Reader Int String
add2AndShow = do
  i <- ask
  return (show (i + 2))

- The *runReader* function, as you might expect, allows us to
  run a Reader action by passing in the global state. Any kind of
  'runX' function typically allows you to extract a "pure" result
  from a monadic comutation by passing in the required initial state.

runReader :: Reader r a -> r -> a

- Let's run our example from above. We pass the action itself as the first parameter
  and then an initial value.

runReader add2AndShow 4 -> "6"

- Within any do-syntax segment, you can use a 'let' statement on one line
  to define a value without it needing to be a monadic action.

add2AndShow :: Reader Int String
add2AndShow = do
  i <- ask
  let newSum = i + 2
  return (show newSum)

- Two other 'Reader' functions you can use are 'asks' and 'local'.
  The first lets you run a function on the global value while accessing it.
  For example, you might access a field of a larger object. Then 'local'
  lets you run a separate computation with a modification of your state object.
  The updated state only affects the separate computation. Subsequent computations
  will use the original state.

asks :: (r -> a) -> Reader r a
local :: (r -> r) -> Reader r a -> Reader r a

-}

add2AndShow :: Reader Int String
add2AndShow = do
  i <- ask
  let newSum = i + 2
  return (show newSum)

-- TODO: Fill in the functions below, including 3 related to the 'User' type!

-- Same as add2AndShow, but return a concatenated string with both the
-- result on the original int as well as twice it's value!
-- runReader add2AndShowDouble 3 -> "(5,8)"
add2AndShowDouble :: Reader Int String
add2AndShowDouble = ???

data User = User
  { userEmail :: String
  , userPassword :: String
  , userName :: String
  , userAge :: Int
  , userBio :: String
  }

-- Validate that the entered password is equal to the stored user's password.
validateAccount :: String -> String -> Reader User Bool
validateAccount = ???

-- Given a user, display the lines of a "profile page" of the form:
-- Name: {name}
-- Age: {age}
-- Bio: {bio}
displayProfile :: Reader User [String]
displayProfile = ???

-- Given a User, and the entered credentials, return their profile
-- string if the authentication is valid, or otherwise return nothing.
authAndDisplayProfile :: User -> (String, String) -> Maybe [String]
authAndDisplayProfile = ???

-- Test Code

u1 :: User
u1 = User "john@test.com" "password" "John Smith" 35 "John is a Doctor at the Hospital"

u2 :: User
u2 = User "kate@test.com" "abcdefgh" "Kate Smith" 32 "Kate writes code."

main :: IO ()
main = defaultMain $ testGroup "Monads3" $
  [ testCase "add2AndShowDouble 1" $ runReader add2AndShowDouble 1 @?= "3,4"
  , testCase "add2AndShowDouble 2" $ runReader add2AndShowDouble 7 @?= "9,16"
  , testCase "Validate Account 1" $ runReader (validateAccount "john@test.com" "password") u1 @?= True
  , testCase "Validate Account 2" $ runReader (validateAccount "john@test.com" "abcdefgh") u1 @?= False
  , testCase "Validate Account 3" $ runReader (validateAccount "john@test.com" "password") u2 @?= False
  , testCase "Validate Account 4" $ runReader (validateAccount "kate@test.com" "password") u2 @?= False
  , testCase "Validate Account 5" $ runReader (validateAccount "kate@test.com" "abcdefgh") u2 @?= True
  , testCase "Validate Account 6" $ runReader (validateAccount "kate@test.com" "abcdefgh") u1 @?= False
  , testCase "Display Profile 1" $ runReader displayProfile u1 @?= ["Name: John Smith", "Age: 35", "Bio: John is a Doctor at the Hospital"]
  , testCase "Display Profile 2" $ runReader displayProfile u2 @?= ["Name: Kate Smith", "Age: 32", "Bio: Kate writes code."]
  , testCase "Auth and Display Profile 1" $ authAndDisplayProfile u1 ("john@test.com", "password") @?=
      Just ["Name: John Smith", "Age: 35", "Bio: John is a Doctor at the Hospital"]
  , testCase "Auth and Display Profile 2" $ authAndDisplayProfile u1 ("john@test.com", "abcdefgh") @?= Nothing
  , testCase "Auth and Display Profile 3" $ authAndDisplayProfile u2 ("john@test.com", "password") @?= Nothing
  , testCase "Auth and Display Profile 4" $ authAndDisplayProfile u2 ("kate@test.com", "password") @?= Nothing
  , testCase "Auth and Display Profile 5" $ authAndDisplayProfile u2 ("kate@test.com", "abcdefgh") @?=
      Just ["Name: Kate Smith", "Age: 32", "Bio: Kate writes code."]
  , testCase "Auth and Display Profile 6" $ authAndDisplayProfile u1 ("kate@test.com", "abcdefgh") @?= Nothing
  ]

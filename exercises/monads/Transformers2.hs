import Control.Monad.Trans.Maybe
import Data.List
import Text.Read

{-

- Since 'Maybe' is a monad as well, it also has a monad transformer 'MaybeT'.
  There's no 'runMaybe' function though. You just use 'MaybeT' as a constructor
  aroud a function in the underlying monad.

newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a) }

maybeAction :: MaybeT IO String
maybeAction = MaybeT $ do
  input <- getLine    -- Normal IO monad actions!
  return (Just input) -- Must return a value as 'Maybe'

- We can see that 'runMaybeT' is the name of the 'field', so we can
  also use it as a function

callFromIO :: IO String
callFromIO = do
  result <- runMaybeT maybeAction
  case result of
    Nothing -> return "Default"
    Just s -> return s

- The 'MaybeT' transformer retains the semantics of the normal 'Maybe' monad.
  If one operation returns 'Nothing', the computation stops!

-}

data User = User
  { email :: String
  , password :: String
  , age :: Int
  } deriving (Show)

-- TODO:
--
-- Fill in these functions to populate a user with the validated information!
-- You should print the given prompt and then use 'getLine' to retrieve the
-- input, applying the specified validation and return 'Nothing' if it
-- doesn't pass.

-- Must contain the '@' and '.' characters.
-- Prompt: "Please enter your email"
readEmail :: MaybeT IO String
readEmail = MaybeT $ do
  putStrLn "Please enter your email"
  s <- getLine
  let validated = f s
  case validated of
    True ->
      return $ Just s
    False -> 
      return Nothing
  where
    f :: String -> Bool
    f s = '@' `elem` s && '.' `elem` s


-- Must be at least 8 characters
-- Prompt: "Please enter your password"
readPassword :: MaybeT IO String
readPassword = MaybeT $ do
  putStrLn "Please enter your password"
  s <- getLine
  let validated = f s
  case validated of
    True ->
      return $ Just s
    False -> 
      return Nothing
  where
    f :: String -> Bool
    f s = length s >= 8


-- Should pass 'readMaybe' for an Int
-- Prompt: "Please enter your age"
readAge :: MaybeT IO Int
readAge = MaybeT $ do
  putStrLn "Please enter your age"
  s <- getLine
  let validated = f s
  case validated of
    True ->
      return $ Just $ (read s :: Int)
    False -> 
      return Nothing
  where
    f :: String -> Bool
    f s = case (readMaybe s :: Maybe Int) of
            Just _ -> True
            Nothing -> False


-- Apply the functions above to produce a 'User'
readUser :: MaybeT IO User
readUser = MaybeT $ do
  email <- runMaybeT readEmail
  case email of
    Just e -> do
      password <- runMaybeT readPassword
      case password of 
        Just p -> do
          age <- runMaybeT readAge
          case age of
            Just a -> return $ (Just User { email = e, password = p, age = a })
            Nothing -> return Nothing
        Nothing -> return Nothing
    Nothing -> return Nothing

-- Call your 'readUser' function on two different users,
-- printing the results each time.
main :: IO ()
main = do
  user1 <- runMaybeT readUser
  putStrLn $ show user1
  user2 <- runMaybeT readUser
  putStrLn $ show user2

{-

Sample Input:
stephanie@test.com
password
31
john@test.com
short

Sample Output:
Please enter your email
Please enter your password
Please enter your age
Just (User {email = "stephanie@test.com", password = "password", age = 31})
Please enter your email
Please enter your password
Nothing

-}

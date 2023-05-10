import Text.Read

{-

- Once we know how to print items to the terminal, we need to know how
  to retrieve a user's input. This is done with the 'getLine' function:

getLine :: IO String

main = do
  -- (String)    (IO String)
  usersInput  <- getLine

- When reading non-string data, you'll likely have to use 'read' or
  some other parsing function. However, 'read' by itself will through
  a runtime error if the input string doesn't match our type.

-- Can't read "Hello" as an Integer!
badRead :: Int
badRead = read "Hello"

- To fix this, use 'readMaybe'. Then you can provide a more appropriate
  failure mechanism.

readMaybe :: (Read a) => String -> Maybe a

-}

-- TODO:

-- Prompt the user to enter a number by printing "Please enter a number."
-- Then retrieve an integer entered by a user.
-- If it can be read as an integer properly, print "Received x" where x is the number.
-- Otherwise, print "Could not read that as an integer." and return 'Nothing'.
readSingleInteger :: IO (Maybe Int)
readSingleInteger = do
  putStrLn "Please enter a number."
  input <- getLine
  case readMaybe input of
    Just num -> do
      putStrLn $ "Received " ++ show num
      return (Just num)
    Nothing -> do
      putStrLn "Could not read that as an integer."
      return Nothing

-- Start by retrieving two numbers. If they both succeed, print their sum
-- with "The sum of these is x." Then read another number and print the
-- final sum in the same format.
-- If any input fails, instead print "Sum is not possible." and exit.
main :: IO ()
main = do
  maybeNum1 <- readSingleInteger
  maybeNum2 <- readSingleInteger

  -- Calculate the sum if both inputs are valid
  case (maybeNum1, maybeNum2) of
    (Just num1, Just num2) -> do
      let sum = num1 + num2
      putStrLn $ "The sum of these is " ++ show sum ++ "."

      -- Read another number
      maybeNum3 <- readSingleInteger

      -- Print the final sum if the input is valid
      case maybeNum3 of
        Just num3 -> do
          let finalSum = sum + num3
          putStrLn $ "The sum of these is " ++ show finalSum ++ "."
        Nothing -> putStrLn "Sum is not possible."
    _ -> putStrLn "Sum is not possible."

{-

Sample Input:
4
5
"Hello"

Sample Output:
Please enter a number.
Received 4
Please enter a number.
Received 5
The sum of these is 9.
Please enter a number.
Could not read that as an integer.
Sum is not possible.

-}

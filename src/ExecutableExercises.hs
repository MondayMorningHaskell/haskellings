module ExecutableExercises where

import           Data.List (isPrefixOf)

-- Predicates for Executable exercises

io1Pred :: [String] -> Bool
io1Pred output = case output of
  [o1, o2, o3, o4] ->
    o1 == "Hello, World!" &&
    isPrefixOf "Running from directory:" o2 &&
    isPrefixOf "Home directory is:" o3 &&
    isPrefixOf "Home directory contains " o4
  _ -> False

io2Inputs :: [String]
io2Inputs = ["4", "5", "Hello"]

io2Pred :: [String] -> Bool
io2Pred = (==)
  [ "Please enter a number."
  , "Received 4"
  , "Please enter a number."
  , "Received 5"
  , "The sum of these is 9."
  , "Please enter a number."
  , "Could not read that as an integer."
  , "Sum is not possible."
  ]

io3Pred :: [String] -> Bool
io3Pred = (==)
  [ "[\"First line!\",\"Second line.\",\"Third line...\",\"Final line!\"]"
  , "First line!"
  ]

transformers1Inputs :: [String]
transformers1Inputs = ["2.0", "Add 4.0", "Multiply 6.0", "Sqrt"]

transformers1Pred :: [String] -> Bool
transformers1Pred = (==)
  [ "Please enter a number."
  , "Please enter three operations."
  , "Adding 4.0"
  , "Multiplying by 6.0"
  , "Taking Square Root"
  , "Result: (6.0,26)"
  ]

transformers2Inputs :: [String]
transformers2Inputs =
  ["stephanie@test.com", "password", "31", "john@test.com", "short"]

transformers2Pred :: [String] -> Bool
transformers2Pred = (==)
  [ "Please enter your email"
  , "Please enter your password"
  , "Please enter your age"
  , "Just (User {email = \"stephanie@test.com\", password = \"password\", age = 31})"
  , "Please enter your email"
  , "Please enter your password"
  , "Nothing"
  ]

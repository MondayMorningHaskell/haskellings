module ExecutableExercises where

import Data.List (isPrefixOf)

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
  , "Received 4."
  , "Please enter a number."
  , "Received 5."
  , "The sum of these is 9."
  , "Please enter a number."
  , "Could not read that as an integer."
  , "Sum is not possible."
  ]

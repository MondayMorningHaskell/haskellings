{-|
Module      : Haskellings.Internal.ExecutableExercises
Description : Contains inputs and expected outputs for executable exercises.
License     : BSD3
Maintainer  : james@mondaymorninghaskell.me

To check each "executable" exercise, we run it with a list of inputs, and then
verify that the line-by-line output satisfies a particular predicate. This might
be that the lines precisely match what is in this file, or some lines might
only have prefixes.
-}

module Haskellings.Internal.ExecutableExercises (
  -- * Exercise IO1
    io1Pred
  -- * Exercise IO2
  , io2Inputs
  , io2Pred
  -- * Exercise IO3
  , io3Pred
  -- * Exercise Transformers1
  , transformers1Inputs
  , transformers1Pred
  -- * Exercise Transformers2
  , transformers2Inputs
  , transformers2Pred
) where

import           Data.List (isPrefixOf)

-- Predicates for Executable exercises

-- | Predicate for exercise IO1. Mostly matches on prefixes.
io1Pred :: [String] -> Bool
io1Pred output = case output of
  [o1, o2, o3, o4] ->
    o1 == "Hello, World!" &&
    isPrefixOf "Running from directory:" o2 &&
    isPrefixOf "Home directory is:" o3 &&
    isPrefixOf "Home directory contains " o4
  _ -> False

-- | Inputs for exercise IO2.
io2Inputs :: [String]
io2Inputs = ["4", "5", "Hello"]

-- | Predicate for exercise IO2. Exact match.
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

-- | Predicate for exercise IO3. Exact match.
io3Pred :: [String] -> Bool
io3Pred = (==)
  [ "[\"First line!\",\"Second line.\",\"Third line...\",\"Final line!\"]"
  , "First line!"
  ]

-- | Inputs for exercise Transformers1
transformers1Inputs :: [String]
transformers1Inputs = ["2.0", "Add 4.0", "Multiply 6.0", "Sqrt"]

-- | Predicate for exercise Transformers1. Exact match.
transformers1Pred :: [String] -> Bool
transformers1Pred = (==)
  [ "Please enter a number."
  , "Please enter three operations."
  , "Adding 4.0"
  , "Multiplying by 6.0"
  , "Taking Square Root"
  , "Result: (6.0,26)"
  ]

-- | Inputs for exercise Transformers2
transformers2Inputs :: [String]
transformers2Inputs =
  ["stephanie@test.com", "password", "31", "john@test.com", "short"]

-- | Predicate for exercise Transformers2. Exact match.
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

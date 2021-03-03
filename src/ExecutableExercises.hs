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

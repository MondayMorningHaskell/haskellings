module ExerciseList where

import qualified Data.Map as M

data ExerciseInfo = ExerciseInfo
  { exerciseName       :: String
  , exerciseDirectory  :: String
  , exerciseIsRunnable :: Bool
  , exerciseHint       :: String
  } deriving (Show, Eq)

allExercises :: [ExerciseInfo]
allExercises =
  [ ExerciseInfo "Types1" "types" False "myFirstVariable is a number. What numeric type can you fill in?"
  , ExerciseInfo "Types2" "types" False "What are the component types of aTuple? Are the values in aList really 'Int'?"
  , ExerciseInfo "Recursion1" "recursion" True "Start with a base pattern of [], and then define how you would incorporate the first element in the list with the recursive result."
  ]

allExercisesMap :: M.Map String ExerciseInfo
allExercisesMap = M.fromList
  [(exerciseName ex, ex) | ex <- allExercises]

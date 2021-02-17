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
  [ ExerciseInfo "Expressions" "basics" True "Fill in numeric values in place of '???' for expression3 and expression4."
  , ExerciseInfo "Types1" "basics" False "Fill in appropriate type signatures for the expressions at the bottom."
  , ExerciseInfo "Types2" "basics" False "Fill in the missing appropriate type signatures and values at the bottom."
  , ExerciseInfo "Types3" "basics" False "Fill in the missing appropriate type signatures and values at the bottom."
  , ExerciseInfo "Types4" "basics" False "Which element of mixedList doesn't belong? How can you fix it?"
  , ExerciseInfo "Types5" "basics" False "What happens when you change 'String' to '[Char]' and vice-versa?"
  , ExerciseInfo "Recursion1" "recursion" True "Start with a base pattern of [], and then define how you would incorporate the first element in the list with the recursive result."
  ]

allExercisesMap :: M.Map String ExerciseInfo
allExercisesMap = M.fromList
  [(exerciseName ex, ex) | ex <- allExercises]

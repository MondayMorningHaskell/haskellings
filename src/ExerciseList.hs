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
  [ ExerciseInfo "Expressions" "basics" False "Fill in numeric values in place of '???' for expression3 and expression4."
  , ExerciseInfo "Types1" "basics" False "Fill in appropriate type signatures for the expressions at the bottom."
  , ExerciseInfo "Types2" "basics" False "Fill in the missing appropriate type signatures and values at the bottom."
  , ExerciseInfo "Types3" "basics" False "Fill in the missing appropriate type signatures and values at the bottom."
  , ExerciseInfo "Types4" "basics" False "Which element of mixedList doesn't belong? How can you fix it?"
  , ExerciseInfo "Types5" "basics" False "What happens when you change 'String' to '[Char]' and vice-versa?"
  , ExerciseInfo "Functions1" "functions" True "Fill in the missing appropriate type signatures and values at the bottom, following the earlier examples in the module."
  , ExerciseInfo "Functions2" "functions" True "When taking a tuple as an input, you can assign a name to each element of the tuple: capitalize (c1, c2, c3) = ..."
  , ExerciseInfo "Functions3" "functions" True "Define the second function by a partial application of the first."
  , ExerciseInfo "Functions4" "functions" True "Remember to keep parentheses around the operator when defining it!"
  , ExerciseInfo "Functions5" "functions" True "For the 'sink' functions, think about partial operator application!"
  , ExerciseInfo "Functions6" "functions" True "Remember that a String is just a list of characters: [Char]."
  , ExerciseInfo "Syntax1" "syntax" True "There is no 'elif' like in Python. Use 'else' and then another full 'if' statement."
  , ExerciseInfo "Syntax2" "syntax" True "You don't need a guard branch for every possible case in countTrue!"
  , ExerciseInfo "Syntax3" "syntax" True "For your catch-all case, either use an expression name like normal for the input, or use an underscore '_' if you don't need the input."
  , ExerciseInfo "Syntax4" "syntax" True "Split the function up with the boolean and then use a 'case' statement!"
  , ExerciseInfo "Syntax5" "syntax" True "Split the problem into small chunks, and make an expression for each chunk in the 'where' clause!"
  , ExerciseInfo "Recursion1" "recursion" True "Start with a base pattern of [], and then define how you would incorporate the first element in the list with the recursive result."
  ]

allExercisesMap :: M.Map String ExerciseInfo
allExercisesMap = M.fromList
  [(exerciseName ex, ex) | ex <- allExercises]

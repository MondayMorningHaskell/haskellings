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
  , ExerciseInfo "Syntax6" "syntax" True "Keeping the start of your definitions aligned helps make 'let' clauses much more readable!"
  , ExerciseInfo "Data1" "data" False "Once you know the types you need, it's simple to add them to a constructor!"
  , ExerciseInfo "Data2" "data" True "On 'giveFullName', remember to use pattern matching, and that '++' can append strings together!"
  , ExerciseInfo "Data3" "data" False "You can follow your own conventions with spacing and new lines when defining the record names."
  , ExerciseInfo "Data4" "data" True "Use pattern matching to deal with different sizes of lists."
  , ExerciseInfo "Data5" "data" False "What should the underlying types be for each name given in the type signatures?"
  , ExerciseInfo "Data6" "data" True "How can you quickly change the type synonyms to newtypes? What errors are revealed?"
  , ExerciseInfo "Typeclasses1" "typeclasses" True "Use a comma separated list within parens to constrain on multiple classes."
  , ExerciseInfo "Typeclasses2" "typeclasses" True "Remember the class name, type name, and keyword 'where' in instance definitions!"
  , ExerciseInfo "Typeclasses3" "typeclasses" True "The last function should be polymorphic in both inputs, with different constraints on each!"
  , ExerciseInfo "Typeclasses4" "typeclasses" True "You can use one typeclass function to help with another! Just don't create a dependency loop between them!"
  , ExerciseInfo "Typeclasses5" "typeclasses" True "Make use of the existing 'Show' and 'Read' instances of the types."
  , ExerciseInfo "Recursion1" "recursion" True "Remember the base case! Also remember that 'mod' can help you determine if a number is even."
  , ExerciseInfo "Recursion2" "recursion" True "1. Sometimes there is no work and only a recursive call! 2. Pattern matches can reveal more than just the head of a list!"
  , ExerciseInfo "Recursion3" "recursion" True "You can use multiple accumulator arguments if you want!"
  , ExerciseInfo "Recursion4" "recursion" True "You can't technically use 'tail recursion' but you'll still want a helper function, at least on the second function!"
  , ExerciseInfo "Lists1" "lists" True "Tail recursion should provide you with some inspiration for your folding functions!"
  ]

allExercisesMap :: M.Map String ExerciseInfo
allExercisesMap = M.fromList
  [(exerciseName ex, ex) | ex <- allExercises]

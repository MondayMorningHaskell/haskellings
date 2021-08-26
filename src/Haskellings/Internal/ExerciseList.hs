{-|
Module      : Haskellings.Internal.ExerciseList
Description : Definitive list of exercises in Haskellings
License     : BSD3
Maintainer  : james@mondaymorninghaskell.me

This module contains all the ExerciseInfo objects. Each of these refers
to a single exercise in Haskellings, complete with a module name, a
directory path, the "type" of exercise, and a hint.
-}

module Haskellings.Internal.ExerciseList (
    allExercises
  , allExercisesMap
) where

import qualified Data.Map                                 as M

import           Haskellings.Internal.ExecutableExercises
import           Haskellings.Types

-- | All Haskellings exercises, listed in the expected order a user would do them.
--   (The Watcher uses this ordering)
allExercises :: [ExerciseInfo]
allExercises =
  [ ExerciseInfo "Expressions" "basics" CompileOnly "Fill in numeric values in place of '???' for expression3 and expression4."
  , ExerciseInfo "Types1" "basics" CompileOnly "Fill in appropriate type signatures for the expressions at the bottom."
  , ExerciseInfo "Types2" "basics" CompileOnly "Fill in the missing appropriate type signatures and values at the bottom."
  , ExerciseInfo "Types3" "basics" CompileOnly "Fill in the missing appropriate type signatures and values at the bottom."
  , ExerciseInfo "Types4" "basics" CompileOnly "Which element of mixedList doesn't belong? How can you fix it?"
  , ExerciseInfo "Types5" "basics" CompileOnly "What happens when you change 'String' to '[Char]' and vice-versa?"
  , ExerciseInfo "Functions1" "functions" UnitTests "Fill in the missing appropriate type signatures and values at the bottom, following the earlier examples in the module."
  , ExerciseInfo "Functions2" "functions" UnitTests "When taking a tuple as an input, you can assign a name to each element of the tuple: capitalize (c1, c2, c3) = ..."
  , ExerciseInfo "Functions3" "functions" UnitTests "Define the second function by a partial application of the first."
  , ExerciseInfo "Functions4" "functions" UnitTests "Remember to keep parentheses around the operator when defining it!"
  , ExerciseInfo "Functions5" "functions" UnitTests "For the 'sink' functions, think about partial operator application!"
  , ExerciseInfo "Functions6" "functions" UnitTests "Remember that a String is just a list of characters: [Char]."
  , ExerciseInfo "Curry" "functions" UnitTests "Use 'uncurry' and 'curry' and remember their type signatures."
  , ExerciseInfo "Syntax1" "syntax" UnitTests "There is no 'elif' like in Python. Use 'else' and then another full 'if' statement."
  , ExerciseInfo "Syntax2" "syntax" UnitTests "You don't need a guard branch for every possible case in countUnitTests!"
  , ExerciseInfo "Syntax3" "syntax" UnitTests "For your catch-all case, either use an expression name like normal for the input, or use an underscore '_' if you don't need the input."
  , ExerciseInfo "Syntax4" "syntax" UnitTests "Split the function up with the boolean and then use a 'case' statement!"
  , ExerciseInfo "Syntax5" "syntax" UnitTests "Split the problem into small chunks, and make an expression for each chunk in the 'where' clause!"
  , ExerciseInfo "Syntax6" "syntax" UnitTests "Keeping the start of your definitions aligned helps make 'let' clauses much more readable!"
  , ExerciseInfo "Data1" "data" CompileOnly "Once you know the types you need, it's simple to add them to a constructor!"
  , ExerciseInfo "Data2" "data" UnitTests "On 'giveFullName', remember to use pattern matching, and that '++' can append strings together!"
  , ExerciseInfo "Data3" "data" CompileOnly "You can follow your own conventions with spacing and new lines when defining the record names."
  , ExerciseInfo "Data4" "data" UnitTests "Use pattern matching to deal with different sizes of lists."
  , ExerciseInfo "Data5" "data" CompileOnly "What should the underlying types be for each name given in the type signatures?"
  , ExerciseInfo "Data6" "data" UnitTests "How can you quickly change the type synonyms to newtypes? What errors are revealed?"
  , ExerciseInfo "Typeclasses1" "typeclasses" UnitTests "Use a comma separated list within parens to constrain on multiple classes."
  , ExerciseInfo "Typeclasses2" "typeclasses" UnitTests "Remember the class name, type name, and keyword 'where' in instance definitions!"
  , ExerciseInfo "Typeclasses3" "typeclasses" UnitTests "The last function should be polymorphic in both inputs, with different constraints on each!"
  , ExerciseInfo "Typeclasses4" "typeclasses" UnitTests "You can use one typeclass function to help with another! Just don't create a dependency loop between them!"
  , ExerciseInfo "Typeclasses5" "typeclasses" UnitTests "Make use of the existing 'Show' and 'Read' instances of the types."
  , ExerciseInfo "Recursion1" "recursion" UnitTests "Remember the base case! Also remember that 'mod' can help you determine if a number is even."
  , ExerciseInfo "Recursion2" "recursion" UnitTests "1. Sometimes there is no work and only a recursive call! 2. Pattern matches can reveal more than just the head of a list!"
  , ExerciseInfo "Recursion3" "recursion" UnitTests "You can use multiple accumulator arguments if you want!"
  , ExerciseInfo "Recursion4" "recursion" UnitTests "You can't technically use 'tail recursion' but you'll still want a helper function, at least on the second function!"
  , ExerciseInfo "Lists1" "lists" UnitTests "Tail recursion should provide you with some inspiration for your folding functions!"
  , ExerciseInfo "Lists2" "lists" UnitTests "Your countdown doesn't need to end exactly with 0."
  , ExerciseInfo "Lists3" "lists" UnitTests "Remember 3 steps for a comprehension: 1. source 2. filter 3. result."
  , ExerciseInfo "Lists4" "lists" UnitTests "Remember 'show' and string appending!"
  , ExerciseInfo "Functors" "monads" UnitTests "You can use the underlying functor instances when creating your functor!"
  , ExerciseInfo "Applicatives" "monads" UnitTests "You can use 'fmap' (<$>) to wrap a function in the Applicative before applying <*>!"
  , ExerciseInfo "Monoids" "monads" UnitTests "Think about what you would use as the identity element for each item!"
  , ExerciseInfo "Monads1" "monads" UnitTests "The bind operator (>>=) allows us to pass values from one monadic function to another without needing to unwrap in between!"
  , ExerciseInfo "Monads2" "monads" UnitTests "The key is understanding what goes on each side of '<-' operator. The wrapped computation goes on the right, the unwrapped result goes on the left."
  , ExerciseInfo "Monads3" "monads" UnitTests "You can still use an 'if' inside do-syntax! Just make sure that both branches are monadic actions!"
  , ExerciseInfo "Monads4" "monads" UnitTests "You'll have to wrap the string values in their own lists to use the monoid instance!"
  , ExerciseInfo "Monads5" "monads" UnitTests "Remember that 'execState' will discard the computation result!"
  , ExerciseInfo "IO1" "monads" (Executable [] io1Pred) "Remember how to use do-syntax to retrieve the results of computations!"
  , ExerciseInfo "IO2" "monads" (Executable io2Inputs io2Pred) "Using fmap (<$>) and the bind operator (>>=) can help you write fewer lines of code!"
  , ExerciseInfo "IO3" "monads" (Executable [] io3Pred) "Remember you can either read the file all at once or line-by-line!"
  , ExerciseInfo "Transformers1" "monads" (Executable transformers1Inputs transformers1Pred) "Understand how 'runStateT' extends the idea of 'runState'."
  , ExerciseInfo "Transformers2" "monads" (Executable transformers2Inputs transformers2Pred) "You can combine 'MaybeT' actions in the same do-syntax block!"
  ]

-- | All Haskellings exercises, stored as a map. This allows us to easily
--   reference an exercise when using 'haskellings run'.
allExercisesMap :: M.Map String ExerciseInfo
allExercisesMap = M.fromList
  [(exerciseName ex, ex) | ex <- allExercises]

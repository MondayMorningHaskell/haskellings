module Data3 where

-- I AM NOT DONE

{-

- Suppose we have this basic type for an assignment grade:

data Grade = Grade Int Char String

- The only way to "unwrap" the values from an object of this type is to
  run a pattern match, unless we write extra function to do the unwrapping for us:

comments :: Grade -> String
comments (Grade _ _ c) = c

- Doing this for every field of every type would be tedious though! So we can let
  Haskell derive these functions for us by using *Record Syntax*. In this way it's
  possible to assign a name to each field so we can use it for future reference.
  You need to use curly braces with this syntax. It is often wise to prefix the
  field names with the (lowercase) type name to prevent conflicts.

data Grade = Grade
  { gradeScore :: Int
  , gradeLetterScore :: Char
  , gradeComments :: String
  }

- It is possible, though not usually advisable to have multiple constructors
  with record syntax. This is likely to result in functions that are invalid
  on certain constructors.

-}

-- TODO: Recreate your Adult and Child types from 'Data1', as separate types.
--       However, you should now use record syntax to name each of the type fields.

-- Notice how we can initialize this type using the field names
adult1 :: Adult
adult1 = Adult
  { adultFirstName = "John"
  , adultLastName = "Smith"
  , adultAge = 45
  , adultOccupation = "Lawyer"
  }

-- However, you can also use the old way, without the field names.
adult2 :: Adult
adult2 = Adult "Jane" "Smith" 39 "Engineer"

child1 :: Child
child1 = Child
  { childName = "Christopher"
  , childAge = 9
  , childGrade = 4
  }

child2 :: Child
child2 = Child "Stephanie" 12 6

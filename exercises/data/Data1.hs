module Data1 where

-- I AM NOT DONE

{-

- In Haskell we can define our own data types with the 'data' keyword. We use that,
  give our type a capitalized name, and then give it a *constructor*.
- The constructor also has a capitalized name (which might be the same as the type
  name), and is then followed by any number of Types that are necessary to
  construct the type.

- Our 'Person' type here has a name (a String) and an age (Int)

data Person = Person String Int

- That's all there is to it!

-}

-- TODO:

-- Define two types, where the constructor's name matches the type name.
-- First, define "Adult", which will have 4 fields:
-- A first name (String)
-- A last name (String)
-- An age (Int)
-- A job (String)

-- Second, define "Child", which will have 3 fields:
-- A first name (String)
-- An age (Int)
-- A "grade" level (Int)

adult1 :: Adult
adult1 = Adult "John" "Smith" 45 "Lawyer"

adult2 :: Adult
adult2 = Adult "Jane" "Smith" 39 "Engineer"

child1 :: Child
child1 = Child "Christopher" 9 4

child2 :: Child
child2 = Child "Stephanie" 12 6

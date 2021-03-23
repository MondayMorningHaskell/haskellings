module Expressions where

-- I AM NOT DONE

{- 
- In Haskell, you can define *expressions* by taking a name and using the
  "equals" sign to assign it a value. See how we do this with
  "expression1" and "expression2" below.
-}

expression1 = 1

expression2 = 4.5

-- TODO: To make this module compile, replace the ??? marks with different
-- numeric values.

expression3 = ???

expression4 = ???

{- 
- Haskell expression aren't "variables" like you might have in other languages.
  Once a value is assigned, we can't change it later. Thus there can be only
  one definition for each name.
-}

-- Try uncommenting this "redefinition" of expression1. In another language,
-- this might "redefine" the variable, but in Haskell, you'll get a
-- compilation error!

-- TODO: Try uncommenting me and compiling! (remove the dashes)
-- expression1 = 2
-- (and then re-comment ^^ to compile again)

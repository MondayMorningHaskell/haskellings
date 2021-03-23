module Types1 where

-- I AM NOT DONE

{-
- In Haskell, every expression has a *type*.
  With *type inference*, the compiler can usually determine the
  type of an expression without us telling it, which happened in
  the last exercise.

- But when defining top-level expressions, we usually want to provide
  an explicity *type signature*. We do this by re-writing the name
  of the expression, using two colons, and then giving the type name.
  "Int" and "Float" are two of the most basic numeric types we can use.
-}

expression1 :: Int
expression1 = 1

expression2 :: Float
expression2 = 4.5

{-
- There are many different numeric types:

  Word - Unsigned integer (>= 0)

  Int64  - 64 bit signed integer
  Word16 - 16 bit unsigned integer
         NOTE: Can use sizes 8, 16, 32, and 64 for both Int and Word
               Int and Word can be either 32 or 64 bit (system dependent)

  Double - Double precision floating point number

  Integer - *Unbounded* integer type
-}


-- TODO: Provide types for each of the expressions below. Of the example types
-- given, how many work for each one?
expression3 :: ???
expression3 = 50

expression4 :: ???
expression4 = 100.1

expression5 :: ???
expression5 = -32

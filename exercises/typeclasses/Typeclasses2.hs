import Test.Tasty
import Test.Tasty.HUnit

{-

- Many times, we don't want to (or can't) use the default instances we get 
  from using 'deriving'. In these cases we'll have to define our own instances.
  In this case we use the 'instance' keyword like so:

data Person = Person String Int

instance Show Person where
  ...

- Then within the instance, we have to provide a function implementation for each
  function within the class.

instance Show Person where
  show (Person name salary) = name ++ " makes $" ++ show salary ++ " per year."

- Some classes have a "minimal complete definition". This means we don't have to
  define every function, because it can derive some functions from the others.

- For example, with Eq, we only have to implement (==) and not (/=), because it
  can use the definition of (==) to fill in (/=)

instance Eq Person where
  (==) (Person n1 _) (Person n2 _) = n1 == n2 -- < Ignore salaries

-}

-- TODO: Write manual definitions of 'Show' and 'Eq' for these types, with
--       the denoted changes from the default definitions.

-- For the 'Show' instance, use lowercase for the first letter.
data Occupation = Lawyer | Programmer | Engineer | Doctor | Manager | Teacher

instance Show Occupation where
  show Lawyer = "lawyer"
  show Programmer = "programmer"
  show Engineer = "engineer"
  show Doctor = "doctor"
  show Manager = "manager"
  show Teacher = "teacher"

instance Eq Occupation where
  (==) Lawyer Lawyer = True
  (==) Programmer Programmer = True
  (==) Engineer Engineer = True
  (==) Doctor Doctor = True
  (==) Manager Manager = True
  (==) Teacher Teacher = True
  (==) _ _ = False

-- Consider two 'Persons' "equal" as long as the constructor and name matches.
-- Use the first two string fields for Adults and the first field for Children
-- For 'Show', you should only use the full name and age:
--   "John Smith is 32 years old"
--   "Chris is 12 years old"
data Person =
  Adult String String Int Occupation |
  Child String Int Int

instance Show Person where
  show (Adult firstName lastName age _) = firstName ++ " " ++ lastName ++ " is " ++ show age ++ " years old"
  show (Child firstName age _) = firstName ++ " is " ++ show age ++ " years old"

instance Eq Person where
  (==) (Adult firstName1 lastName1 _ _) (Adult firstName2 lastName2 _ _) = firstName1 == firstName2 && lastName1 == lastName2
  (==) (Child firstName1 _ _) (Child firstName2 _ _) = firstName1 == firstName2
  (==) _ _ = False

-- The 'Show' instance should omit 'InterestRate'. Just show the underlying Double.
newtype InterestRate = InterestRate Double

instance Show InterestRate where
  show (InterestRate double) = show double

instance Eq InterestRate where
  (==) (InterestRate double1) (InterestRate double2) = double1 == double2

main :: IO ()
main = defaultMain $ testGroup "Typeclasses2" $
  [ testCase "Show Occupation" $ map show [Lawyer, Programmer] @?= ["lawyer", "programmer"]
  , testCase "Eq Occupation" $ map (uncurry (==)) [(Lawyer, Engineer), (Engineer, Engineer), (Doctor, Doctor)] @?= [False, True, True]
  , testCase "Show Person 1" $ show (Adult "John" "Smith" 32 Doctor) @?= "John Smith is 32 years old"
  , testCase "Show Person 2" $ show (Adult "Jane" "Smith" 31 Teacher) @?= "Jane Smith is 31 years old"
  , testCase "Show Person 3" $ show (Child "Chris" 12 7) @?= "Chris is 12 years old"
  , testCase "Eq Person 1" $ (Adult "John" "Smith" 32 Doctor) == (Adult "John" "Smith" 35 Engineer) @?= True
  , testCase "Eq Person 2" $ (Adult "John" "Smith" 32 Doctor) == (Adult "John" "Adams" 32 Doctor) @?= False
  , testCase "Eq Person 3" $ (Child "Chris" 12 7) == (Adult "Chris" "Smith" 35 Teacher) @?= False
  , testCase "Eq Person 4" $ (Child "Chris" 12 7) == (Child "Chris" 11 6) @?= True
  , testCase "Eq Person 5" $ (Child "Chris" 12 7) == (Child "Christine" 12 7) @?= False
  , testCase "Show Interest Rate" $ map show [InterestRate 0.5, InterestRate 0.8] @?= ["0.5", "0.8"]
  , testCase "Eq Interest Rate" $ map (uncurry (==)) [(InterestRate 0.3, InterestRate 0.3), (InterestRate 0.3, InterestRate 0.5)] @?= [True, False]
  ]

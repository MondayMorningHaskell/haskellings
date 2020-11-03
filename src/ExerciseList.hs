module ExerciseList where

import qualified Data.Map as M

exerciseMap :: M.Map String ExerciseInfo
exerciseMap = M.fromList
  [ ("Types1", ExerciseInfo "Types1" "types" "Types1.hs" False "myFirstVariable is a number. What numeric type can you fill in?")
  , ("Types2", ExerciseInfo "Types2" "types" "Types2.hs" False "What are the component types of aTuple? Are the values in aList really 'Int'?")
  , ("Recursion1", ExerciseInfo "Recursion1" "recursion" "Recursion1.hs" True "Start with a base pattern of [], and then define how you would incorporate the first element in the list with the recursive result.")
  ]

data ExerciseInfo = ExerciseInfo
  { exerciseName :: String
  , exerciseDirectory :: String
  , exerciseModuleFile :: String
  , exerciseIsRunnable :: Bool
  , exerciseHint :: String
  } deriving (Show, Eq)

exerciseList :: [ExerciseInfo]
exerciseList =
  [ ExerciseInfo "Types1" "types" "Types1.hs" False "myFirstVariable is a number. What numeric type can you fill in?"
  , ExerciseInfo "Types2" "types" "Types2.hs" False "What are the component types of aTuple? Are the values in aList really 'Int'?"
  , ExerciseInfo "Recursion1" "recursion" "Recursion1.hs" True "Start with a base pattern of [], and then define how you would incorporate the first element in the list with the recursive result."
  ]

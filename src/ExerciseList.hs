module ExerciseList where

import qualified Data.Map as M

exerciseMap :: M.Map String ExerciseInfo
exerciseMap = M.fromList
  [ ("Types1", ExerciseInfo "Types1" "types" "Types1.hs" False)
  , ("Types2", ExerciseInfo "Types2" "types" "Types2.hs" False)
  , ("Recursion1", ExerciseInfo "Recursion1" "recursion" "Recursion1.hs" True)
  ]

data ExerciseInfo = ExerciseInfo
  { exerciseName :: String
  , exerciseDirectory :: String
  , exerciseModuleFile :: String
  , exerciseIsRunnable :: Bool
  } deriving (Show, Eq)

exerciseList :: [ExerciseInfo]
exerciseList =
  [ ExerciseInfo "Types1" "types" "Types1.hs" False
  , ExerciseInfo "Types2" "types" "Types2.hs" False
  , ExerciseInfo "Recursion1" "recursion" "Recursion1.hs" True
  ]

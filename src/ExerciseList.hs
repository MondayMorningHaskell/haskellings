module ExerciseList where

import qualified Data.Map as M

exerciseMap :: M.Map String ExerciseInfo
exerciseMap = M.fromList
  [ ("Types1", ExerciseInfo "Types1" "types" "Types1.hs")
  , ("Types2", ExerciseInfo "Types2" "types" "Types2.hs")
  ]

data ExerciseInfo = ExerciseInfo
  { exerciseName :: String
  , exerciseDirectory :: String
  , exerciseModuleFile :: String
  } deriving (Show, Eq)

exerciseList :: [ExerciseInfo]
exerciseList =
  [ ExerciseInfo "Types1" "types" "Types1.hs"
  , ExerciseInfo "Types2" "types" "Types2.hs"
  ]

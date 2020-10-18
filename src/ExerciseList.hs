module ExerciseList where

import qualified Data.Map as M

exerciseMap :: M.Map String (String, String)
exerciseMap = M.fromList
  [ ("Types1", ("types", "Types1.hs"))
  , ("Types2", ("types", "Types2.hs"))
  ]

type ExerciseInfo = (String, String, String)

exerciseList :: [ExerciseInfo]
exerciseList =
  [ ("Types1", "types", "Types1.hs")
  , ("Types2", "types", "Types2.hs")
  ]

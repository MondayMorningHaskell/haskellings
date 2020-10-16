module ExerciseList where

import qualified Data.Map as M

exerciseMap :: M.Map String (String, String)
exerciseMap = M.fromList
  [ ("Types1", ("types", "Types1.hs"))
  ]

{- Constants used throughout the program,
   often related to configuration.
-}
module Haskellings.Constants where

import           Data.Maybe
import           System.Environment

ghcVersion :: String
ghcVersion = "ghc-8.10.4"

ghcVersionNumber :: String
ghcVersionNumber = "8.10.4"

projectRootDirName :: String
projectRootDirName = "haskellings"

configFileName :: String
configFileName = "config.yaml"

-- On CircleCI, the root directory shows up as "project'
ciEnvName :: String
ciEnvName = "HASKELLINGS_CI_ENV"

envIsCi :: IO Bool
envIsCi = isJust <$> lookupEnv ciEnvName

ciProjectRootDirName :: String
ciProjectRootDirName = "project"

haskellingsVersion :: String
haskellingsVersion = "0.8.0.0"

mainProjectExercisesDir :: String
mainProjectExercisesDir = "exercises"

-- A listing of packages required by exercises, so we can use them
-- to filter Stack snapshots
haskellingsRequiredLibs :: [String]
haskellingsRequiredLibs =
  [ "tasty"
  , "tasty-hunit"
  ]

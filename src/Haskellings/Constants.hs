{-|
Module      : Haskellings.Constants
Description : Constant expressions for Haskellings
License     : BSD3
Maintainer  : james@mondaymorninghaskell.me

This module has constant values that are used across Haskellings.
It includes things like the string for the GHC version currently in use,
hard-coded directory and file names, and required libraries.
-}

module Haskellings.Constants
( -- * Version strings
  -- | Includes the GHC version currently in use, and the Haskellings version.
    ghcVersion
  , ghcVersionNumber
  , haskellingsVersion
  -- * Files and directories
  -- | These help us locate the exercises and configurations.
  , projectRootDirName
  , mainProjectExercisesDir
  , configFileName
  -- * Required Libraries
  -- | Haskellings requires these are installed with the Stack package
  --   in order to function.
  , haskellingsRequiredLibs
  -- * CI Constants
  -- | These help us to run tests in the Circle CI environment.
  , envIsCi
  , ciProjectRootDirName
) where

import           Data.Maybe
import           System.Environment

-- | The GHC version currently used by Haskellings. We use this
--   to locate the appropriate GHC executable.
ghcVersion :: String
ghcVersion = "ghc-8.10.4"

-- | The version number, isolated from any prefix. Also helps in
--   finding certain directories.
ghcVersionNumber :: String
ghcVersionNumber = "8.10.4"

-- | The current Haskellings program version.
haskellingsVersion :: String
haskellingsVersion = "0.9.0.0"

-- | The project root directory name. We need to find the project root
--   in order to locate the exercises.
projectRootDirName :: String
projectRootDirName = "haskellings"

-- | The name of the exercises directory.
mainProjectExercisesDir :: String
mainProjectExercisesDir = "exercises"

-- | The name of the config file, which can store a custom GHC location.
configFileName :: String
configFileName = "config.yaml"

-- | A listing of packages required by exercises, so we can use them
--   to filter Stack snapshots
haskellingsRequiredLibs :: [String]
haskellingsRequiredLibs =
  [ "tasty"
  , "tasty-hunit"
  ]

-- | Tells us if we are running in a Circle CI environment. This affects
--   how we construct the path and find the exercises.
envIsCi :: IO Bool
envIsCi = isJust <$> lookupEnv ciEnvName

-- | The project root name when running on Circle CI.
ciProjectRootDirName :: String
ciProjectRootDirName = "project"

---------- PRIVATE FUNCTIONS ----------

-- We set this environment variable on Circle CI.
ciEnvName :: String
ciEnvName = "HASKELLINGS_CI_ENV"

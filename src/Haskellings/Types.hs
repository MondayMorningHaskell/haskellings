{-|
Module      : Haskellings.Types
Description : Core types for Haskellings project
License     : BSD3
Maintainer  : james@mondaymorninghaskell.me

Includes types related to configuration and types for
describing exercises.
-}

{-# LANGUAGE OverloadedStrings #-}

module Haskellings.Types (
  -- * Config Types
    ConfigError(..)
  , BaseConfig(..)
  , ProgramConfig(..)
  -- * Exercise Types
  , RunResult(..)
  , ExerciseType(..)
  , ExerciseInfo(..)
  -- * Utility Types
  , FileLockMap
) where

import           Control.Concurrent
import           Data.Aeson
import qualified Data.Map           as M
import           System.IO

-- | Errors we might encounter when constructing the Project Configuration.
data ConfigError
  -- | When we cannot locate the 'haskellings' project root directory.
  = NoProjectRootError
  -- | When we cannot locate the GHC executable
  | NoGhcError
  -- | When we cannot locate the Stack package path
  | NoStackPackageDbError
  deriving (Show)

-- | A type for paths we might store in the config.yaml file made from 'haskellings configure'
data BaseConfig = BaseConfig
  { baseConfigGhcPath   :: Maybe FilePath -- ^ A custom path for the GHC executable
  , baseConfigStackPath :: Maybe FilePath -- ^ A custom path for the Stack package path
  }

instance ToJSON BaseConfig where
  toJSON (BaseConfig ghc stackPackageDb) = object
    [ "ghc_path" .= ghc
    , "stack_package_db_path" .= stackPackageDb
    ]

instance FromJSON BaseConfig where
  parseJSON = withObject "BaseConfig" $ \o -> do
    ghc <- o .:? "ghc_path"
    stackPackageDb <- o .:? "stack_package_db_path"
    return $ BaseConfig ghc stackPackageDb

-- | The primary program configuration object.
--   Contains important directorys and file handles for testing.
data ProgramConfig = ProgramConfig
  { projectRoot  :: FilePath    -- ^ Project root directory absolute path
  , ghcPath      :: FilePath    -- ^ GHC executable absolute path
  , packageDb    :: FilePath    -- ^ Stack package DB absolute path
  , exercisesExt :: FilePath    -- ^ Relative path to exercises directory
  , inHandle     :: Handle      -- ^ Input handle (stdin normally, but can be a file for tests).
  , outHandle    :: Handle      -- ^ Standard output handle (stdout normally, but can be a file for tests).
  , errHandle    :: Handle      -- ^ Error output handle (stderr normally, but can be a file for tests).
  , fileLocks    :: FileLockMap -- ^ Map for locking files that are in use. Primarily for testing.
  }

-- | Possible results of running an exercise.
data RunResult
  = CompileError -- ^ Exercise fails to compile.
  | TestFailed   -- ^ Exercise compiles, but unit tests fail.
  | RunSuccess   -- ^ Exercise succeeds.
  deriving (Show, Eq)

{-| Possible exercise types.
    There are three types of exercises.
    1. Some succeed once they have compiled (CompileOnly).
    2. Some require that unit tests pass on the exercise functions (UnitTests)
    3. Others involve some degree of IO. The User can use 'haskellings exec' on
       these. There is one set of inputs and expected outputs as a quick test,
       when the exercise is run via 'haskellings run' or the Watcher.
-}
data ExerciseType
  -- | Exercise that only needs to compile to pass.
  = CompileOnly
  -- | Exercise with normal unit tests
  | UnitTests
  -- | Executable exercises. One list of expected input lines and a predicate for output lines.
  | Executable [String] ([String] -> Bool)

-- Manual instances needed because 'Executable' contains a function.
instance Eq ExerciseType where
  CompileOnly == CompileOnly = True
  UnitTests == UnitTests = True
  (Executable _ _) == (Executable _ _) = True
  _ == _ = False

instance Show ExerciseType where
  show CompileOnly      = "CompileOnly"
  show UnitTests        = "UnitTests"
  show (Executable _ _) = "Executable"

-- | Information for a single exercise
data ExerciseInfo = ExerciseInfo
  { exerciseName      :: String       -- ^ Name of the module
  , exerciseDirectory :: String       -- ^ Directory containing the exercise
  , exerciseType      :: ExerciseType -- ^ Type of the exercise
  , exerciseHint      :: String       -- ^ Hint string to print for exercise
  } deriving (Show, Eq)

-- | Alias for tracking an MVar for certain files.
--   Ensures unit tests do not alter the same file concurrently.
type FileLockMap = M.Map FilePath (MVar ())

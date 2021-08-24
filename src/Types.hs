{- Core types for project. -}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Concurrent
import           Data.Aeson
import qualified Data.Map           as M
import           System.IO

data ConfigError = NoProjectRootError | NoGhcError | NoStackPackageDbError
  deriving (Show)

type FileLockMap = M.Map FilePath (MVar ())

data BaseConfig = BaseConfig
  { baseConfigGhcPath   :: Maybe FilePath
  , baseConfigStackPath :: Maybe FilePath
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

data ProgramConfig = ProgramConfig
  { projectRoot  :: FilePath
  , ghcPath      :: FilePath
  , packageDb    :: FilePath
  , exercisesExt :: FilePath
  , inHandle     :: Handle
  , outHandle    :: Handle
  , errHandle    :: Handle
  , fileLocks    :: FileLockMap
  }

data RunResult =
  CompileError | TestFailed | RunSuccess
  deriving (Show, Eq)

-- There are three types of exercises.
-- 1. Some succeed once they have compiled (CompileOnly).
-- 2. Some require that unit tests pass on the exercise functions (UnitTests)
-- 3. Others involve some degree of IO. The User can use 'haskellings exec' on
--    these. There is one set of inputs and expected outputs as a quick test,
--    when the exercise is run via 'haskellings run' or the Watcher.
data ExerciseType =
  CompileOnly |
  UnitTests |
  -- One set of expected input lines and predicate for output lines.
  Executable [String] ([String] -> Bool)

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

data ExerciseInfo = ExerciseInfo
  { exerciseName      :: String
  , exerciseDirectory :: String
  , exerciseType      :: ExerciseType
  , exerciseHint      :: String
  } deriving (Show, Eq)

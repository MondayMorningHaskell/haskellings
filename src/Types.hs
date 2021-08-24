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

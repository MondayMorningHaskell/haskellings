{-|
Module      : Haskellings.TerminalUtils
Description : Utility functions for printing to the terminal.
License     : BSD3
Maintainer  : james@mondaymorninghaskell.me

These functions allow printing to the terminal, but use a ProgramConfig
so that test functions can use files for input and output. Also allows
coloring terminal output to be green or red.
-}

module Haskellings.TerminalUtils (
  -- * Basic Printing with Program Config
  progPutStr,
  progPutStrLn,
  progPrint,
  progPutStrErr,
  progPrintErr,
  progReadLine,
  -- * Success and Failure Functions
  withTerminalSuccess,
  withTerminalFailure,
  progPutStrLnSuccess,
  progPutStrLnFailure
) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           System.Console.ANSI
import           System.IO

import           Haskellings.Types

-- | Print the given string without a new line.
progPutStr :: String -> ReaderT ProgramConfig IO ()
progPutStr str = do
  handle <- asks outHandle
  lift $ hPutStr handle str

-- | Print the given string with a new line.
progPutStrLn :: String -> ReaderT ProgramConfig IO ()
progPutStrLn str = do
  handle <- asks outHandle
  lift $ hPutStrLn handle str

-- | Print the Show-able object with a new line.
progPrint :: (Show a) => a -> ReaderT ProgramConfig IO ()
progPrint val = do
  handle <- asks outHandle
  lift $ hPrint handle val

-- | Print the given string (with a new line) to the error stream.
progPutStrErr :: String -> ReaderT ProgramConfig IO ()
progPutStrErr str = do
  handle <- asks errHandle
  lift $ hPutStrLn handle str

-- | Print the Show-able object (with a new line) to the error stream.
progPrintErr :: (Show a) => a -> ReaderT ProgramConfig IO ()
progPrintErr val = do
  handle <- asks errHandle
  lift $ hPrint handle val

-- | Read a line using the Program Config's input handler.
progReadLine :: ReaderT ProgramConfig IO String
progReadLine = asks inHandle >>= (lift . hGetLine)

-- | Perform an action with 'Green' terminal text.
withTerminalSuccess :: (MonadIO m) => m a -> m a
withTerminalSuccess = withTerminalColor Green

-- | Perform an action with 'Red' terminal text.
withTerminalFailure :: (MonadIO m) => m a -> m a
withTerminalFailure = withTerminalColor Red

-- | Print a line, but with Green color.
progPutStrLnSuccess :: String -> ReaderT ProgramConfig IO ()
progPutStrLnSuccess output = withTerminalSuccess (progPutStrLn output)

-- | Print a line, but with Red color.
progPutStrLnFailure :: String -> ReaderT ProgramConfig IO ()
progPutStrLnFailure output = withTerminalFailure (progPutStrLn output)

---------- PRIVATE FUNCTIONS ----------

-- Perform an action with printed output given a color.
withTerminalColor :: (MonadIO m) => Color -> m a -> m a
withTerminalColor color action = do
  liftIO $ setSGR [SetColor Foreground Vivid color]
  res <- action
  liftIO $ setSGR [Reset]
  return res

{- Utility functions for printing to the terminal.
   Deal with different kinds of Handles, adds color.
-}
module Haskellings.TerminalUtils where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           System.Console.ANSI
import           System.IO

import           Haskellings.Types

progPutStr :: String -> ReaderT ProgramConfig IO ()
progPutStr str = do
  handle <- asks outHandle
  lift $ hPutStr handle str

progPutStrLn :: String -> ReaderT ProgramConfig IO ()
progPutStrLn str = do
  handle <- asks outHandle
  lift $ hPutStrLn handle str

progPrint :: (Show a) => a -> ReaderT ProgramConfig IO ()
progPrint val = do
  handle <- asks outHandle
  lift $ hPrint handle val

progPutStrErr :: String -> ReaderT ProgramConfig IO ()
progPutStrErr str = do
  handle <- asks errHandle
  lift $ hPutStrLn handle str

progPrintErr :: (Show a) => a -> ReaderT ProgramConfig IO ()
progPrintErr val = do
  handle <- asks errHandle
  lift $ hPrint handle val

progReadLine :: ReaderT ProgramConfig IO String
progReadLine = asks inHandle >>= (lift . hGetLine)

-- Perform an action with 'Green' Terminal Text
withTerminalSuccess :: (MonadIO m) => m a -> m a
withTerminalSuccess = withTerminalColor Green

-- Perform an action with 'Red' Terminal Text
withTerminalFailure :: (MonadIO m) => m a -> m a
withTerminalFailure = withTerminalColor Red

-- Perform an action with printed output given a color.
withTerminalColor :: (MonadIO m) => Color -> m a -> m a
withTerminalColor color action = do
  liftIO $ setSGR [SetColor Foreground Vivid color]
  res <- action
  liftIO $ setSGR [Reset]
  return res

-- Print a line, but in Green
progPutStrLnSuccess :: String -> ReaderT ProgramConfig IO ()
progPutStrLnSuccess output = withTerminalSuccess (progPutStrLn output)

-- Print a line, but in Red
progPutStrLnFailure :: String -> ReaderT ProgramConfig IO ()
progPutStrLnFailure output = withTerminalFailure (progPutStrLn output)

module TerminalUtils where

import           System.Console.ANSI
import           System.IO

import           Types

progPutStr :: ProgramConfig -> String -> IO ()
progPutStr pc = hPutStr (outHandle pc)

progPutStrLn :: ProgramConfig -> String -> IO ()
progPutStrLn pc = hPutStrLn (outHandle pc)

progPrint :: (Show a) => ProgramConfig -> a -> IO ()
progPrint pc = hPrint (outHandle pc)

progPutStrErr :: ProgramConfig -> String -> IO ()
progPutStrErr pc = hPutStrLn (errHandle pc)

progPrintErr :: (Show a) => ProgramConfig -> a -> IO ()
progPrintErr pc = hPrint (errHandle pc)

progReadLine :: ProgramConfig -> IO String
progReadLine pc = hGetLine (inHandle pc)

-- Perform an action with 'Green' Terminal Text
withTerminalSuccess :: IO a -> IO a
withTerminalSuccess = withTerminalColor Green

-- Perform an action with 'Red' Terminal Text
withTerminalFailure :: IO a -> IO a
withTerminalFailure = withTerminalColor Red

-- Perform an action with printed output given a color.
withTerminalColor :: Color -> IO a -> IO a
withTerminalColor color action = do
  setSGR [SetColor Foreground Vivid color]
  res <- action
  setSGR [Reset]
  return res

-- Print a line, but in Green
progPutStrLnSuccess :: ProgramConfig -> String -> IO ()
progPutStrLnSuccess pc output = withTerminalSuccess (progPutStrLn pc output)

-- Print a line, but in Red
progPutStrLnFailure :: ProgramConfig -> String -> IO ()
progPutStrLnFailure pc output = withTerminalFailure (progPutStrLn pc output)

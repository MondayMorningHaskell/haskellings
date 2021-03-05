-- I AM NOT DONE

import System.Directory
import System.FilePath
import System.IO

{-

- Reading files is an important function of many programs. In Haskell, these
  operations must take place in the IO Monad. The most simple functions allow
  us to manipulate the whole file at once:

-- Read a file's contents into a single string
readFile :: FilePath -> IO String

-- Write a file to a string, overwriting any existing information
writeFile :: String -> FilePath -> IO ()

- The *Handle* abstraction let's us do more advanced operations. We can
  use 'openFile' to a file, either in 'ReadMode', 'WriteMode', or
  'AppendMode'.

openFile :: FilePath -> IOMode -> IO Handle

- Once we have a file handle, we can read or write with it line-by-line
  just like with the terminal!

-- Only works with 'ReadMode'!
hGetLine :: Handle -> IO String

-- Works with 'WriteMode' or 'AppendMode'
hPutStrLn :: Handle -> String -> IO ()

- When you're done with a Handle, you should close it with 'hClose'!

hClose :: Handle -> IO ()

- There are also a couple very useful Prelude functions to convert between
  strings with newlines and lists of strings.

lines   :: String -> [String]
unlines :: [String] -> String

unlines ["Hello", "World"] -> "Hello\nWorld"
lines "Hello\nWorld" -> ["Hello", "World"]

-}

writeToFile :: FilePath -> IO ()
writeToFile fp = writeFile fp (unlines ["First line!", "Second line.", "Third line..."])

addExtraLine :: FilePath -> IO ()
addExtraLine fp = do
  handle <- openFile fp AppendMode
  hPutStrLn handle "Final line!"
  hClose handle

-- TODO: Write a couple simple functions to read the file's contents!

-- Return a list of all the strings in the file!
getAllLines :: FilePath -> IO [String]
getAllLines fp = ???

-- Read only the first line in the file! Use a 'Handle'!
readFirstLine :: FilePath -> IO String
readFirstLine fp = ???

-- NOTE: This will create a file in your home directory,
--       but delete it once it successfully runs.
main :: IO ()
main = do
  home <- getHomeDirectory
  let fp = home </> "haskellings_io3.txt"
  writeToFile fp
  addExtraLine fp
  getAllLines fp >>= print
  readFirstLine fp >>= putStrLn
  removeFile fp

{-

Sample Output:

["First line!","Second line.","Third line...","Final line!"]
First line!

-}

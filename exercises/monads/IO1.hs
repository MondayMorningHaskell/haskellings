-- I AM NOT DONE

import System.Directory

{-

- The *IO* monad is perhaps the most important to know about in Haskell.
  It's also the most special, carrying a different kind of effect to other monads.

- Most functions we run are "pure". Their output can't be influenced by
  anything besides the direct input values we use, whether explicitly as
  arguments, or implicitly through a Stateful monad.

- They also don't produce additional side effects. They can provide a
  return value to the rest of our program, and they can have an implicit
  output through the Writer or State monad.

- The *IO* monad provides the computation context of
  *interaction with the outside world*.

- This means it allows us to take additional inputs like reading from the
  terminal, opening up a file from the OS, or reading a network connection.

- The IO monad is different from other monads in that there is no 'runIO' function.
  You *cannot* run 'IO' code from non-IO code. The entrypoint for any program
  you write is the 'main' function. This has the type 'IO ()':

main :: IO ()
main = do
  ...

- You can run non-IO code from this function using 'let' or 'where' clauses,
  like we've already seen in monads. But any 'IO' code must be called from
  a chain of other 'IO' code going back to this function.

- In this exercise, you'll practice using a few basic 'IO' functions:

-- Prints a 'String' object to the terminal.
putStrLn :: String -> IO ()

-- Prints any 'Show' object to the terminal.
-- (Note, 'print'-ing a 'String' will cause it to appear with extra quotation marks)
-- (Hello vs. "Hello")
print :: (Show a) -> a -> IO ()

-- A 'FilePath' is just an alias for a 'String'
-- This function retrieves the directory the program is being run from.
getCurrentDirectory :: IO FilePath

-- This retrieves the "Home" directory path on your system.
getHomeDirectory :: IO FilePath

-- This provides a list of all the files and directories within the input.
-- (Like running the 'ls' or 'dir' command)
listDirectory :: FilePath -> IO [FilePath]

-}

-- TODO:

-- Print four messages to the terminal.
-- 1. First use putStrLn to produce the string "Hello, World!"
-- 2. Then print "Running from directory:" plus the "current" directory.
-- 3. Next, find the home directory and print it out: "Home directory is: ..."
-- 4. Finally, list the home directory and print how many elements it has:
--    "Home directory contains 10 sub-paths."
--
-- To run this code without evaluating it, use 'haskellings exec IO1'
main :: IO ()
main = ???

{-

Sample Output:

Hello, World!
Running from directory: /home/myuser/haskellings
Home directory is: /home/myuser
Home directory contains 10 sub-paths.

-}

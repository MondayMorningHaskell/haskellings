-- I AM NOT DONE

main :: IO ()
main = do
  putStrLn "Hello, please enter your name!"
  name <- getLine
  putStrLn $ "Hello, " ++ name

{-

Sample Input:

John

Sample Output:

Hello, please enter your name!
Hello, John

-}

-- I AM NOT DONE

sumList :: [Int] -> Int
sumList _ = 5

main :: IO ()
main = do
  let firstSum = sumList [1, 2, 3, 4]
  let secondSum = sumList [2, 3, 5, 6]
  if firstSum /= 10 || secondSum /= 16
    then error ("Sums were incorrect! Expected 10 and 16 but got: " ++ show firstSum ++ " and " ++ show secondSum)
    else return ()

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (h:t) = quickSort (filter (< h) t) ++ [h] ++ quickSort (filter (>= h) t)

main = do print (quickSort [3,2,4,5,1,2,3,4,1,5])
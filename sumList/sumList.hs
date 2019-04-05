sumList :: [Int] -> Int
sumList [] = 0
sumList x  = head x + sumList (tail x)

-- main = print (sumList [1,2,3,4])


-- dobrar os elementos 
double :: [Int] -> [Int]
double [] = []
double (h:t)  = 2*h : double t

-- verifica se elemento está na lista
member :: [Int] -> Int -> Bool
member [] el  = False
member (h:t) el = (h == el) || (member t el)

-- apenas números em uma string
digits :: String -> String
digits []    = []
digits (h:t) | h >= '0' && h <= '9' = h : digits t
             | otherwise = digits t

-- soma de uma lista de pares
-- sumPairs :: [(Int, Int)] -> [Int]



main = do print (member [1,2,3,4] 3)
          print (digits "sapdofij9d8fjdifja9")
       
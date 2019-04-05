-- str -> baseChar -> objectiveChar -> response
replace :: String -> Char -> Char -> String
replace [] _ _ = ""
replace (h:t) c r 
  | h == c = [r] ++ replace t c r
  | otherwise = [h] ++ replace t c r

_splitBy :: [String] -> [String] -> Int -> ([String], [String])
_splitBy acc [] _ = (acc, [])
_splitBy acc (h:t) order
  | length acc == order = (acc, [h]++t)
  | otherwise = _splitBy (acc++[h]) t order

splitBy :: [String] -> Int -> ([String], [String])
splitBy lst order = _splitBy [] lst order
  
findEntries :: [String] -> Int -> [[String]]
findEntries [] _ = [] 
findEntries lst order = [fst (splitBy lst 4)] ++ findEntries (snd (splitBy lst 4)) 4

process :: String -> [[String]]
process str = findEntries (words (replace str ';' ' ')) 4

-- Resolution
logMes :: String -> String -> Double
logMes db mes = foldr (+) 0 [read (node!!3) :: Double | node <- (process db), node!!1 == mes]

logCartao = "14 JAN;Amazon;40.32;15 JAN;Uber;14.84;25 JAN;Uber;34.24;02 FEV;Spotify;8.50;06 FEV;Uber;6.94;05 MAR;Burger;29.90;10 MAR;Burger;24.99;15 MAR;UCI;19.00;08 ABR;Itunes;3.50;13 ABR;Picpay;20.00;"

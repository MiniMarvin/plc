repeatChar :: Char -> Int -> String
repeatChar _ 0 = []
repeatChar c n = [c] ++ (repeatChar c (n-1))

allButLast :: String -> String
allButLast [] = []
allButLast [c] = []
allButLast (c:cs) = [c] ++ (allButLast cs)

getLast :: String -> Char
getLast [] = ' '
getLast [c] = c
getLast (c:cs) = getLast cs

splitByFirstLetter :: String -> String
splitByFirstLetter [] = []
splitByFirstLetter (c:cs)
    | c >= '9' || c <= '0' = [c]
    | otherwise = [c] ++ splitByFirstLetter cs
    
splitByFirstLetterRest :: String -> String
splitByFirstLetterRest [] = []
splitByFirstLetterRest (c:cs)
    | c >= '9' || c <= '0' = cs
    | otherwise = splitByFirstLetterRest cs

splitAllEncoded :: String -> [String]
splitAllEncoded [] = []
splitAllEncoded str = [(splitByFirstLetter str)] ++ splitAllEncoded (splitByFirstLetterRest str)

expandStr :: String -> String
expandStr str = repeatChar (getLast str) (read (allButLast str))

expandAll :: [String] -> String
expandAll [] = []
expandAll (s:ss) = (expandStr s) ++ (expandAll ss)

decode_rle :: String -> String
decode_rle str = expandAll (splitAllEncoded str)


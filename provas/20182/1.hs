splitFirst :: Char -> String -> String
splitFirst _ [] = []
splitFirst ref (c:cs)
    | c == ref = [c] ++ splitFirst c cs
    | otherwise = []

splitSecond :: Char -> String -> String
splitSecond _ [] = []
splitSecond ref (c:cs)
    | c == ref = splitSecond c cs
    | otherwise = (c:cs)

splitAll :: String -> [String]
splitAll [] = []
splitAll (c:cs) = [(splitFirst c (c:cs))] ++ splitAll (splitSecond c (c:cs))

convertStr :: String -> String
convertStr [] = []
convertStr (c:cs) = (show (length (c:cs))) ++ [c]

convertAll :: [String] -> String
convertAll [] = []
convertAll (s:ss) = (convertStr s) ++ (convertAll ss)

encode_rle :: String -> String
encode_rle [] = []
encode_rle str = convertAll (splitAll str)

-- WWWWWWBWWWXYYZZZ

splitByFirstLetter :: String -> String
splitByFirstLetter [] = []
splitByFirstLetter (c:cs)
    | c >= '9' || c <= '0' = []
    | otherwise = [c] ++ splitByFirstLetter cs
    
splitByFirstLetterRest :: String -> String
splitByFirstLetterRest [] = []
splitByFirstLetterRest (c:cs)
    | c >= '9' || c <= '0' = (c:cs)
    | otherwise = splitByFirstLetterRest cs

splitByFirstNumber :: String -> String
splitByFirstNumber [] = []
splitByFirstNumber (c:cs)
    | c <= '9' && c >= '0' = []
    | otherwise = [c] ++ splitByFirstNumber cs
    
splitByFirstNumberRest :: String -> String
splitByFirstNumberRest [] = []
splitByFirstNumberRest (c:cs)
    | c <= '9' && c >= '0' = (c:cs) -- found a letter
    | otherwise = splitByFirstNumberRest cs

splitFirstPair :: String -> String
splitFirstPair [] = []
-- splitFirstPair str = (splitByFirstNumber str) ++ (splitByFirstLetter (splitByFirstNumberRest str))
splitFirstPair str = (splitByFirstNumber str) ++ [(head (splitByFirstNumberRest str))]

splitFirstPairRest :: String -> String
splitFirstPairRest [] = []
-- splitFirstPairRest str = (splitByFirstLetterRest (splitByFirstNumberRest str))
splitFirstPairRest str = (tail (splitByFirstNumberRest str))

splitAllCode :: String -> [String]
splitAllCode [] = []
splitAllCode str = [splitFirstPair str] ++ (splitAllCode (splitFirstPairRest str))

type Dicionario = [(Int, String)]

findKey :: Int -> Dicionario -> String
findKey _ [] = []
findKey k ((key,val):ds)
    | k == key = val
    | otherwise = findKey k ds

translatePair :: String -> Dicionario -> String
translatePair [] _ = []
translatePair _ [] = []
translatePair str dict = (splitByFirstNumber str) ++ (findKey (read (splitByFirstNumberRest str)) dict)

translateAll :: [String] -> Dicionario -> String
translateAll [] _ = []
translateAll _ [] = []
translateAll (s:ss) dict = (translatePair s dict) ++ (translateAll ss dict)

decode :: Dicionario -> String -> String
decode [] _ = []
decode _ [] = []
decode dict str = translateAll (splitAllCode str) dict

-- "a 1 tinha 3 mas nao 6 7 34, a 1 6 7 sorve4"
meuDicionario :: Dicionario
meuDicionario = [(1, "casa"), (3, "cafe"), (4, "teria"), (6, "era"), (7, "uma")]
teste = "a 1 tinha 3 mas nao 6 7 34, a 1 6 7 sorve4"
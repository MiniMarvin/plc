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

-- type Dicionario = [(Int, String)]
type DicionarioT = Tree Int String
data Tree chave valor = Node chave valor (Tree chave valor) (Tree chave valor)
                      | Leaf deriving (Eq, Show)

findKey :: Int -> DicionarioT -> String
findKey v Leaf = []
findKey v (Node x str t1 t2)
    | x == v = str
    | x < v = findKey v t2
    | otherwise = findKey v t1

-- findKey :: Int -> Dicionario -> String
-- findKey _ [] = []
-- findKey k ((key,val):ds)
--     | k == key = val
--     | otherwise = findKey k ds

translatePair :: String -> DicionarioT -> String
translatePair [] _ = []
translatePair _ Leaf = []
translatePair str dict = (splitByFirstNumber str) ++ (findKey (read (splitByFirstNumberRest str)) dict)

translateAll :: [String] -> DicionarioT -> String
translateAll [] _ = []
translateAll _ Leaf = []
translateAll (s:ss) dict = (translatePair s dict) ++ (translateAll ss dict)

decode :: DicionarioT -> String -> String
decode Leaf _ = []
decode _ [] = []
decode dict str = translateAll (splitAllCode str) dict

decodeTree :: DicionarioT -> String -> String
decodeTree Leaf _ = []
decodeTree _ [] = []
decodeTree dict str = translateAll (splitAllCode str) dict

-- decodeTree meuDicionarioT teste
meuDicionarioT :: DicionarioT
meuDicionarioT = Node 4 "teria" (Node 3 "cafe" (Node 1 "casa" Leaf Leaf) Leaf)
                                (Node 6 "era" Leaf (Node 7 "uma" Leaf Leaf))
teste = "a 1 tinha 3 mas nao 6 7 34, a 1 6 7 sorve4"
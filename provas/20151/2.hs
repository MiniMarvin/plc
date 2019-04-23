type Resultado = [Int]
type Jogos = [[Int]]

countLen :: [t] -> Int
countLen [] = 0
countLen (h:t) = 1 + countLen t

-- Get all games
premiados :: Resultado -> Jogos -> Int
premiados r j = countLen [a | a <- j, a == r]

-- let t1 = [[1..6], [3..8], [2..7], [1..3]]

isIn :: Int -> [Int] -> Int
isIn _ [] = 0
isIn a (h:hs)
    | a == h = 1 + isIn a hs
    | otherwise = isIn a hs

-- reference
countEqual :: Resultado -> Resultado -> Int
countEqual _ [] = 0
countEqual [] _ = 0
countEqual (r:rs) comp = (isIn r comp) + countEqual rs comp


acertos :: Resultado -> Jogos -> [Int]
acertos [] _ = []
acertos _ [] = []
acertos r (j:js) = [countEqual r j] ++ (acertos r js)

t0 :: [Int]
t0 = [1..6]

t1 :: [[Int]]
t1 = [[1..6], [3..8], [2..7], [1..6]]



numPremios :: Resultado -> Jogos -> (Int, Int, Int)
numPremios r j = (length [a | a <- acertos r j, a == 4], length [a | a <- acertos r j, a == 5], length [a | a <- acertos r j, a == 6])

data Time = Egito | Russia | Arabia | Uruguai | Ira | Marrocos | Portugal | Espanha deriving (Eq, Show)
type Jogo = (Time, Int, Int, Time) 

-- 3 pontos por uma vitória, 1 por um empate, e zero por uma derrota.
pontos :: Time -> [Jogo] -> Int
pontos _ [] = 0
pontos t ((t1, g1, g2, t2):js)
    | t == t1 && g1 > g2 = 3 + (pontos t js)
    | t == t1 && g1 == g2 = 1 + (pontos t js)
    | t == t1 && g1 < g2 = (pontos t js)
    | t == t2 && g2 > g1 = 3 + (pontos t js)
    | t == t2 && g2 == g1 = 1 + (pontos t js)
    | t == t2 && g2 < g1 = (pontos t js)
    | otherwise = (pontos t js)


jogos1 :: [Jogo]
jogos1 = [(Egito, 1, 3, Russia), (Arabia, 0, 3, Uruguai), 
          (Egito, 0, 0, Arabia),(Russia, 0, 2, Uruguai), 
          (Russia, 2, 0, Arabia), (Egito, 0, 2, Uruguai), 
          (Ira, 1, 1, Marrocos), (Portugal, 2, 2, Espanha), 
          (Ira, 1, 2, Portugal), (Ira, 0, 1, Espanha), 
          (Marrocos, 0, 3, Portugal), (Marrocos, 1, 1, Espanha)]





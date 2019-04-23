data Time = Egito | Russia | Arabia | Uruguai | Ira | Marrocos | Portugal | Espanha deriving (Eq, Show)
type Jogo = (Time, Int, Int, Time) 


-- Egito 3 x 1 Russia será representado por (Egito, 3, 1, Russia)
-- saldo de gols naquele conjunto de jogos (gols feitos - gols tomados)
saldo :: Time -> [Jogo] -> Int
saldo _ [] = 0
saldo t ((t1, g1, g2, t2):js)
    | t == t1 = (g1 - g2) + (saldo t js)
    | t == t2 = (g2 - g1) + (saldo t js)
    | otherwise = saldo t js

jogos1 :: [Jogo]
jogos1 = [(Egito, 1, 3, Russia), (Arabia, 0, 3, Uruguai), 
          (Egito, 0, 0, Arabia),(Russia, 0, 2, Uruguai), 
          (Russia, 2, 0, Arabia), (Egito, 0, 2, Uruguai), 
          (Ira, 1, 1, Marrocos), (Portugal, 2, 2, Espanha), 
          (Ira, 1, 2, Portugal), (Ira, 0, 1, Espanha), 
          (Marrocos, 0, 3, Portugal), (Marrocos, 1, 1, Espanha)]




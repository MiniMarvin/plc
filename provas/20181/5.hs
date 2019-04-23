data Time = Egito | Russia | Arabia | Uruguai | Ira | Marrocos | Portugal | Espanha deriving (Eq, Show)
type Jogo = (Time, Int, Int, Time) 
type Grupo = (Char, Time, Time, Time, Time)

-- Egito 3 x 1 Russia será representado por (Egito, 3, 1, Russia)
-- dado um time e uma lista de jogos, informe quantos gols aquele time fez.
gols :: Time -> [Jogo] -> Int
gols _ [] = 0
gols t ((t1, g1, g2, t2):js)
    | t1 == t = g1 + (gols t js)
    | t2 == t = g2 + (gols t js)
    | otherwise = (gols t js)

-- Egito 3 x 1 Russia será representado por (Egito, 3, 1, Russia)
-- saldo de gols naquele conjunto de jogos (gols feitos - gols tomados)
saldo :: Time -> [Jogo] -> Int
saldo _ [] = 0
saldo t ((t1, g1, g2, t2):js)
    | t == t1 = (g1 - g2) + (saldo t js)
    | t == t2 = (g2 - g1) + (saldo t js)
    | otherwise = saldo t js


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


countPoints :: Time -> [Jogo] -> (Time, Int, Int, Int)
countPoints t js = (t, (pontos t js), (saldo t js), (gols t js))

allPoints :: Grupo -> [Jogo] -> [(Time, Int, Int, Int)]
allPoints _ [] = []
allPoints (name, t1, t2, t3, t4) js = [(countPoints t1 js), (countPoints t2 js), (countPoints t3 js), (countPoints t4 js)]

qsort :: [(Time, Int, Int, Int)] -> [(Time, Int, Int, Int)]
qsort [] = []
qsort ((t, p1, p2, p3):ts) = (qsort [(t, p4, p5, p6) | (t, p4, p5, p6) <- ts, p4 > p1]) ++ 
                             (qsort [(t, p9, p10, p11) | (t, p9, p10, p11) <- ts, p9 == p1, p10 > p2]) ++
                             (qsort [(t, p15, p16, p17) | (t, p15, p16, p17) <- ts, p15 == p1, p16 == p2, p17 > p3]) ++
                             [(t, p1, p2, p3)] ++ 
                             (qsort [(t, p18, p19, p20) | (t, p18, p19, p20) <- ts, p18 == p1, p19 == p2, p20 < p3]) ++
                             (qsort [(t, p12, p13, p14) | (t, p12, p13, p14) <- ts, p12 == p1, p13 < p2]) ++
                             (qsort [(t, p7, p8, p9) | (t, p7, p8, p9) <- ts, p7 < p1])


getTeam :: (Time, Int, Int, Int) -> Time
getTeam (t, _, _, _) = t

classificados :: Grupo -> [Jogo] -> (Time, Time)
classificados g js = (getTeam ((qsort (allPoints g js))!!0), getTeam ((qsort (allPoints g js))!!1))


jogos1 :: [Jogo]
jogos1 = [(Egito, 1, 3, Russia), (Arabia, 0, 3, Uruguai), 
          (Egito, 0, 0, Arabia),(Russia, 0, 2, Uruguai), 
          (Russia, 2, 0, Arabia), (Egito, 0, 2, Uruguai), 
          (Ira, 1, 1, Marrocos), (Portugal, 2, 2, Espanha), 
          (Ira, 1, 2, Portugal), (Ira, 0, 1, Espanha), 
          (Marrocos, 0, 3, Portugal), (Marrocos, 1, 1, Espanha)]
          

g1 :: Grupo
g1 = ('A', Egito, Russia, Arabia, Uruguai)

g2 :: Grupo
g2 = ('B', Ira, Marrocos, Portugal, Espanha)


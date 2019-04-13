-- [Q1] Defina uma função classificados :: Grupo -> [Jogo] -> (Time, Time) que, dado um Grupo e uma lista de jogos, retorne o par de times que estão classificados. Os classificados são: os dois com maior número de pontos; em caso de empate, usa-se o saldo de gols; em caso de continuar empate usa-se o número de gols feitos (há regras adicionais, mas vamos implementar apenas essas 3). Exemplos de grupos são: Grupo A: Egito, Russia, Arabia e Uruguai; Grupo B: Ira, Marrocos, Portugal e Espanha.

type Grupo = (Char, Time, Time, Time, Time)
data Time = Egito | Russia | Arabia | Uruguai |
			Ira | Marrocos | Portugal | Espanha | NilT
			deriving (Show, Eq)
type Jogo = (Time, Int, Int, Time)
type GameResult = (Time, Int, Int, Int) 

-- Pontuação calculada por: 3 pontos na vitoria, 1 ponto no empate e 0 pontos na derrota
-- Saldo de gols: total feito - total recebido
-- Gols feitos
updateResult :: GameResult -> Int -> Int -> Int -> GameResult
updateResult (t, v1, v2, v3) p g1 g2 = (t, v1 + p, v2 + (g1 - g2), v3 + g1)

getResult :: Time -> [GameResult] -> GameResult
getResult t [] = (t, 0, 0, 0) -- In error case returns basically a default value
getResult t ((t1,v1,v2,v3):rs)
	| t == t1 = (t1,v1,v2,v3)
	| otherwise = getResult t rs

updateResults :: [GameResult] -> GameResult -> [GameResult]
updateResults [] _ = []
updateResults ((t1,v1,v2,v3):rs) (t2,v4,v5,v6)
	| t1 == t2 = [(t2,v4,v5,v6)] ++ rs
	| otherwise = [(t1,v1,v2,v3)] ++ (updateResults rs (t2,v4,v5,v6))

groupToResults :: Grupo -> [GameResult]
groupToResults (name, t1, t2, t3, t4) = [(t1, 0, 0, 0), (t2, 0, 0, 0), (t3, 0, 0, 0), (t4, 0, 0, 0)]

-- TODO: Sum every single data into a team set
joinGames :: [Jogo] -> [GameResult] -> [GameResult]
joinGames [] r = r
joinGames ((t1, v1, v2, t2):js) r
	| v1 > v2 = joinGames js (updateResults r1 (updateResult (getResult t2 r1) 0 v2 v1))
	| v1 < v2 = joinGames js (updateResults r2 (updateResult (getResult t2 r2) 3 v2 v1)) 
	| otherwise = joinGames js (updateResults r3 (updateResult (getResult t2 r3) 1 v2 v1)) 
		where 
			r1 = (updateResults r (updateResult (getResult t1 r) 3 v1 v2)) 
			r2 = (updateResults r (updateResult (getResult t1 r) 0 v1 v2)) 
			r3 = (updateResults r (updateResult (getResult t1 r) 1 v1 v2)) 

-- TODO: Quicksort
quickSort :: [GameResult] -> [GameResult]
quickSort [] = []
quickSort ((t, v1, v2, v3):hs) = 
	(quickSort [(t1, v4, v5, v6) | (t1, v4, v5, v6) <- hs, v1 < v4]) ++ 
	(quickSort [(t1, v4, v5, v6) | (t1, v4, v5, v6) <- hs, v1 == v4, v2 < v5]) ++ 
	(quickSort [(t1, v4, v5, v6) | (t1, v4, v5, v6) <- hs, v1 == v4, v2 == v5, v3 < v6]) ++ 
	[(t, v1, v2, v3)] ++ 
	(quickSort [(t1, v4, v5, v6) | (t1, v4, v5, v6) <- hs, v1 == v4, v2 == v5, v3 > v6]) ++ 
	(quickSort [(t1, v4, v5, v6) | (t1, v4, v5, v6) <- hs, v1 == v4, v2 > v5]) ++ 
	(quickSort [(t1, v4, v5, v6) | (t1, v4, v5, v6) <- hs, v1 > v4])

converter :: [GameResult] -> (Time, Time)
converter [] = (NilT, NilT)
converter ((t1, _, _, _):(t2, _, _, _):hs) = (t1, t2)
-- converter ((t1, _, _, _):hs) = (t1, NilT)

classificados :: Grupo -> [Jogo] -> (Time, Time)
classificados grupo jogos = converter (quickSort (joinGames jogos (groupToResults grupo)))




-- Por exemplo: (Egito 3 x 1 Russia) será representado por (Egito, 3, 1, Russia) e o Grupo A seria ('A', Egito, Russia, Arabia, Uruguai) e o Grupo B seria ('B', Ira, Marrocos, Portugal, Espanha]
	  
jogos1 :: [Jogo]
jogos1 = [
	(Egito, 1, 3, Russia), (Arabia, 0, 3, Uruguai), 
	(Egito, 0, 0, Arabia),(Russia, 0, 2, Uruguai), 
	(Russia, 2, 0, Arabia), (Egito, 0, 2, Uruguai), 
	(Ira, 1, 1, Marrocos), (Portugal, 2, 2, Espanha), 
	(Ira, 1, 2, Portugal), (Ira, 0, 1, Espanha), 
	(Marrocos, 0, 3, Portugal), (Marrocos, 1, 1, Espanha)]
		
g1 :: Grupo
g1 = ('A', Egito, Russia, Arabia, Uruguai)

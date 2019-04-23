-- 1) (2.0) Escreva uma funcao locate, que recebe como entrada um elemento e uma lista de elementos, e retorna a localização (o índice) daquele elemento dentro da lista. 
-- A primeira posição na lista tem índice 0 (zero).
-- Caso o elemento não pertença à lista, deve ser retornado o valor (-1).
-- Exemplos: locate 'x' "abcdewxyz" ------>  6
--           locate 5   [5,98,7,32] ------>  0
--           locate True [False, False] --> -1
-- cutList :: t -> [t] -> [t]
-- cutList _ [] = []
-- cutList a (h:hs)
--     | a == h = (h:hs)
--     | otherwise = cutList a hs

locateH :: Eq t => t -> [t] -> Int -> Int
locateH _ [] _ = -1
locateH a (h:hs) acc
    | a == h = acc
    | otherwise = locateH a hs (acc + 1)

locate :: Eq t => t -> [t] -> Int
locate _ [] = -1
locate a lst = locateH a lst 0




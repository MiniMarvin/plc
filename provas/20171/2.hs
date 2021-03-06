-- 2) (2.5) O método de ordenação bubble-sort funciona da seguinte forma: 
--   cada elemento da lista de entrada é comparado com o seguinte, 
--   e se eles não estiverem em ordem (do menor para o maior) sua posição na lista resultante é trocada,
--   e a comparação continua com a nova ordem.Esse processo é repetido até que a lista esteja ordenada 
--   (nenhuma troca seja mais necessária).
--   exemplo, passo a passo: 
--       bSort [4,8,3,6,1,8] ----> compara 4 e 8, 8 e 3 (troca, pois 8 > 3), 8 e 6(troca novamente), 8 e 1 (troca novamente) e 8 e 8  
--                                   ----> [4,3,6,1,8,8]
--       repetindo o processo, temos  ---> [3,4,1,6,8,8] ---> [3,1,4,6,8,8]  ---> [1,3,4,6,8,8]
-- Implemente a função bSort.
-- Dica 1: use funções auxiliares, que façam parte do processo;
-- Dica 2: verifique que sua solução funciona para listas de tamanho ímpar.

bSortH2 :: Ord t => [t] -> [t]
bSortH2 [] = []
bSortH2 [a] = [a]
bSortH2 (a:b:cs)
    | a > b = [b] ++ bSortH2 (a:cs)
    | otherwise = [a] ++ bSortH2 (b:cs)

bSortH1 :: Ord t => [t] -> Int -> [t]
bSortH1 [] _ = []
bSortH1 [a] _ = [a]
bSortH1 lst n
    | (length lst) == n = lst
    | otherwise = bSortH1 (bSortH2 lst) (n + 1)

bSort :: Ord t => [t] -> [t]
bSort lst = bSortH1 lst 0




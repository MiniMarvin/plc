--  1) (2.5) Escreva uma função que verifica se uma lista já está ordenada, 
--    do menor para o maior elemento..
--    exemplo:    ------> True
--             isSorted [1,6,8,7,9] ------> False
--    Dica: verifique se sua resposta funciona para listas de tamanho ímpar.

isSorted :: Ord t => [t] -> Bool
isSorted [] = False
isSorted [a] = True
isSorted (a:b:cs) = (a <= b) && (isSorted (b:cs))
-- 2) (3.0) Escreva uma função que verifique se uma lista está contida em outra (por exemplo, se uma String ésubstring de outra).
-- Exemplos: substr "abc" "xyz12abrt" ----> False
--           substr "abc" "aaabrsabcfr" --> True
--           substr "aab" "aacrtxxeaayb" -> False


substr :: String -> String -> Bool
substr [] _ = True
substr _ [] = False
substr (a:as) (b:bs)
    | a == b = (as) == (take (length (as)) bs) || substr (a:as) bs
    | otherwise = substr (a:as) bs




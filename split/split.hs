-- acc -> string -> character -> (acc, res)
splitFirst :: String -> String -> Char -> (String, String)
splitFirst acc [] _ = (acc, "")
splitFirst acc (h:t) c 
  | h /= c = splitFirst (acc ++ [h]) t c
  | otherwise = (acc, t)

-- string -> param -> [list of strings]
split :: String -> Char -> [String]
split [] _ = [""]
split str c 
  | fst (splitFirst "" str c) == "" = []
  | snd (splitFirst "" str c) == "" = [fst (splitFirst "" str c)]
  | otherwise = [fst (splitFirst "" str c)] ++ split (snd (splitFirst "" str c)) c

-- convert the entire string to the tuple
-- split :: String -> Char -> [String]
-- split [] _ = [""]
-- split (h:t) c = 

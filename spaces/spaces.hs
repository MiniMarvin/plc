genSpaces :: Int -> String
genSpaces 0 = ""
genSpaces i = [' '] ++ genSpaces (i - 1)

addSpaces :: String -> Int -> String
addSpaces (x:xs) i 
  |  xs == [] = [x]
  |  otherwise = [x] ++ genSpaces(i) ++ addSpaces xs i

addSpacesCorrect :: String -> Int -> String
addSpacesCorrect (x:xs) i 
  |  xs == [] = [x]
  |  x  == ' ' = "" ++ addSpaces xs i
  |  otherwise = [x] ++ genSpaces(i) ++ addSpaces xs i
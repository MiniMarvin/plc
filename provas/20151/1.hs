-- map :: (a -> b) -> [a] -> [b]

-- f :: [Integer] -> [Integer -> Integer]
f = map (+)

-- g :: {(a -> b) -> [a] -> [b]} . {(a -> b) -> [a] -> [b]}
-- g :: (a -> b) -> [[a]] -> [[b]]
g = (.) map map


func :: [Integer] -> [Integer -> Integer] -> [Integer]
func [] _ = []
func _ [] = []
func (h1:t1) (h2:t2) = [h2 h1] ++ (func t1 t2)
-- evalTree (Node SUM (Node MUL (Nilt 5) (Nilt 3)) (Node SUB (Nilt 10) (Nilt 5)))
-- 20

data Ops = SUM | SUB | MUL deriving Eq
data IntTree = Nilt Int |
               Node Ops IntTree IntTree

evalTree :: IntTree -> Int
evalTree (Nilt val) = val
evalTree (Node ops t1 t2)
    | ops == SUM = (evalTree t1) + (evalTree t2)
    | ops == SUB = (evalTree t1) - (evalTree t2)
    | otherwise = (evalTree t1) * (evalTree t2)

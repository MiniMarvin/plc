--------------------------------------------------------------------------------
-- exercicio 1
data Expr = Lit Int |
            Add Expr Expr |
            Sub Expr Expr 

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

showExpr :: Expr -> String
showExpr (Lit i) = show i
showExpr (Add a b) = show (eval (Add a b))
showExpr (Sub a b) = show (eval (Sub a b))


--------------------------------------------------------------------------------
-- exercicio 2
data List t = Nil | Cons t (List t) deriving (Show)

toList :: List t -> [t]
toList (Nil) = []
toList (Cons a b) = [a] ++ toList b

-- toList (Cons 7 (Cons 2 (Cons 1 (Nil))))



--------------------------------------------------------------------------------
-- exercicio 3
fromList :: [t] -> List t
fromList [] = Nil
fromList (h:t) = Cons h (fromList t)


--------------------------------------------------------------------------------
-- exercicio 4
data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Show)

depth :: Tree t -> Int
depth NilT = 0
depth (Node a t1 t2) = max (depth t1) (depth t2) + 1

-- depth NilT
-- depth (Node 1 NilT NilT)
-- depth (Node 1 NilT (Node 1 NilT NilT))
-- depth (Node 1 (Node 1 NilT NilT) (Node 1 NilT NilT))

--------------------------------------------------------------------------------
-- exercicio 5
-- Converte uma tree para uma lista
collapse :: Tree t -> [t]
collapse NilT = []
collapse (Node a t1 t2) = (collapse t1) ++ [a] ++ (collapse t2)

-- collapse NilT
-- collapse (Node 1 NilT NilT)
-- collapse (Node 2 NilT (Node 1 NilT NilT))
-- collapse (Node 3 (Node 1 NilT NilT) (Node 2 NilT NilT))

--------------------------------------------------------------------------------
-- exercicio 6
mapTree :: (t -> u) -> Tree t -> Tree u
mapTree f NilT = NilT
mapTree f (Node a t1 t2) = (Node (f a) (mapTree f t1) (mapTree f t2))

-- mapTree (* 2) NilT
-- mapTree (* 2) (Node 1 NilT NilT)
-- mapTree (* 2) (Node 2 NilT (Node 1 NilT NilT))
-- mapTree (* 2) (Node 3 (Node 1 NilT NilT) (Node 2 NilT NilT))



-- exemplo encode "xxxyywzz" -> "3x2y1w2z"
-- exemplo decode "3x2y1w2z" -> "xxxyywzz"

-- TODO: count number of elements
countSequence :: Char -> String -> Int -> Int
countSequence c (h:t) acc
    | c == h = countSequence h
    | otherwise acc 


-- encode :: String -> String


-- decode :: String -> String
-- Exemplos:
-- isBST (Node 5 (Node 3 Nilt Nilt) (Node 7 Nilt Nilt))
-- True
-- isBST (Node 3 (Node 5 Nilt Nilt) (Node 7 Nilt Nilt))
-- False




-- data Tree t = Nilt | Node t (Tree t) (Tree t)
data IntTree = Nilt | Node Int (IntTree) (IntTree) deriving (Eq, Show)

isBST :: IntTree -> Bool
isBST Nilt = True
isBST (Node p (Node s1 t1 t2) (Node s2 t3 t4)) = p < s2 && p > s1 && isBST (Node s1 t1 t2) && isBST (Node s2 t3 t4)
isBST (Node p (Node s1 t1 t2) (Nilt)) = p > s1 && isBST (Node s1 t1 t2) 
isBST (Node p Nilt (Node s2 t3 t4)) = p < s2 && isBST (Node s2 t3 t4)
isBST (Node p Nilt Nilt) = True

-- Exemplo:
-- mapList (*2) (Cons 3 (Cons 2 Nil))
-- Cons 6 (Cons 4 Nil)

data List t = Nil | Cons t (List t) deriving (Eq, Show)


mapList :: (t -> t) -> List t -> List t
mapList f Nil = Nil
mapList f (Cons v lst) = Cons (f v) (mapList f lst)



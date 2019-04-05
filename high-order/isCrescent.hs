func :: Int -> Int
func 0 = 10
func 1 = -20
func 2 = 20
func 3 = 40
func n = 100


isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f v
    | v == 0 = True
    -- | v == 1 = f (v) >= f (v-1)
    | otherwise = f (v) >= f (v-1) && isCrescent f (v-1) 
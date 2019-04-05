-- opcode, origem, destino*, valor
-- opcode = 0 -> crédito
-- opcode = 1 -> débito
-- opcode = 2 -> transferencia
type Transaction = (Int, Int, Int, Double)

-- acc -> values -> objective -> value
getValue :: Int -> [Double] -> Int -> Double
getValue _ [] _ = 0
getValue acc (h:t) obj
    | acc == obj = h
    | otherwise = getValue (acc + 1) t obj

-- acc -> values -> objective -> value -> list
updateIndex :: Int -> [Double] -> Int -> Double -> [Double]
updateIndex acc [] obj value = []
updateIndex acc (h:t) obj value
    | acc == obj = [value] ++ t
    | otherwise = [h] ++ updateIndex (acc+1) t obj value
    
-- créditos contas -> transações -> crédito contas*
processBankOperation :: [Double] -> [Transaction] -> [Double]
processBankOperation [] _ = []
processBankOperation accounts [] = accounts
processBankOperation accounts ((opcode, origin, destine, value):t)
    | opcode == 0 = processBankOperation (updateIndex 0 accounts origin ((getValue 0 accounts origin) + value)) t
    | (opcode == 1) && ((getValue 0 accounts origin) >= value) = processBankOperation (updateIndex 0 accounts origin ((getValue 0 accounts origin) - value)) t
    | opcode == 2 && ((getValue 0 accounts origin) >= value) = processBankOperation (updateIndex 0 (updateIndex 0 accounts origin ((getValue 0 accounts origin) - value)) destine ((getValue 0 accounts destine) + value)) t
    | otherwise = processBankOperation accounts t

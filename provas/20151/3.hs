-- Push: Insere um valor no topo da pilha
-- Pop: Remove o valor do topo da pilha
-- Add: Remove o topo da pilha duas vezes e soma esses dois valores
-- Add: Remove o topo da pilha duas vezes e subtrai esses dois valores
-- Dup: Duplica o valor do topo da pilha
data Instrucao = PUSH Int | POP | ADD | SUB | DUP deriving (Eq, Show)

type Pilha = [Int]

getTop :: Pilha -> Int
getTop [] = 0
getTop (h:hs) = h

removeTop :: Pilha -> Pilha
removeTop [] = []
removeTop (h:hs) = hs

evalI :: Instrucao -> Pilha -> Pilha
evalI (PUSH x) stack = [x] ++ stack 
evalI instruction stack
    | instruction == POP = removeTop stack
    | instruction == ADD = evalI (PUSH ((getTop stack) + (getTop (removeTop stack)))) (removeTop (removeTop stack))
    | instruction == SUB = evalI (PUSH ((getTop stack) - (getTop (removeTop stack)))) (removeTop (removeTop stack))
    | instruction == DUP = evalI (PUSH (getTop stack)) stack
    | otherwise = stack


evalProgC :: [Instrucao] -> Pilha -> Pilha
evalProgC [] stack = stack
evalProgC (h:hs) stack = evalProgC hs (evalI h stack)

evalProg :: [Instrucao] -> Pilha
evalProg lst = evalProgC lst []

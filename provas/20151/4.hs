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



data Expr = Literal Int -- um número
            | Soma Expr Expr -- soma as duas expressões
            | Subtrai Expr Expr -- subtrai a segunda expressão da primeira
            | Dobra Expr -- dobra o valor da expressão


translate :: Expr -> [Instrucao]
translate (Literal a) = [PUSH a]
translate (Soma e1 e2) = (translate e2) ++ (translate e1) ++ [ADD]
translate (Subtrai e1 e2) = (translate e2) ++ (translate e1) ++ [SUB]
translate (Dobra e) = (translate e) ++ [DUP, ADD]


-- translate (Soma (Literal 5) (Dobra (Subtrai (Literal 4) (Literal 1))))
x :: [Instrucao]
x = translate (Soma (Literal 5) (Dobra (Subtrai (Literal 4) (Literal 1))))

-- Q5
-- Avaliação estrita vai computar todos os valores na hora do instanciamento do elemento, 
-- a medida que a avaialção preguiçosa vai avaliar cada elemento de um gerador a medida que
-- for necessário invocar esse elemento.
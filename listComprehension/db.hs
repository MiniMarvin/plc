type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"),
               ("Andre","Duna"),
               ("Fernando","Jonathan Strange & Mr.Norrell"),
               ("Fernando","Duna")]

livros :: BancoDados -> Pessoa -> [Livro]
livros db pessoa = [l | (p, l) <- db, (pessoa, l) == (p, l)]


emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos db livro = [p | (p, l) <- db, (p, livro) == (p, l)]

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver db pessoa livro = [(p, l) | (p, l) <- db, (p,l) /= (pessoa, livro)]

main = print(livros baseExemplo "Andre")
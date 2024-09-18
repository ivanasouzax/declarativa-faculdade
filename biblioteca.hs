-- Sistema de Biblioteca v0.1

-- type Usuario = (Matricula,Nome,Curso)
type Usuario = String
type Usuarios = [Usuario]

type Livro = String
type Livros = [Livro]

type Emprestimo = (Usuario,Livro)
type Emprestimos = [Emprestimo]

emprestimos :: Emprestimos
emprestimos = [ ("Maria","Java"),("Maria","Haskell"),("Rita","Haskell")]

usuarios :: Usuarios
usuarios = ["Maria","Carlos","Rita"]

livros :: Livros
livros = ["Haskell","Java","JavaScript","Pascal"]

obterLivros :: Usuario -> Emprestimos -> Livros
obterLivros usuario [] = []
obterLivros usuario ((n,l):b)
   | usuario == n = l : obterLivros usuario b
   | otherwise = obterLivros usuario b 


registrarEmprestimo :: Livro -> Usuario -> Emprestimos -> Emprestimos
registrarEmprestimo livro usuario emprestimos = (usuario,livro):emprestimos

devolucaoEmprestimo :: Livro -> Usuario -> Emprestimos -> Emprestimos
devolucaoEmprestimo livro usuario [] = []
devolucaoEmprestimo livro usuario ((n,l):b)
    | n == usuario && livro == l = b
    | otherwise = (n,l) : devolucaoEmprestimo livro usuario b


obterUsuarios :: Livro -> Emprestimos -> Usuarios
obterUsuarios livro [] = []
obterUsuarios livro (a:b)
   | livro == livro' = usuario' : obterUsuarios livro b
   | otherwise = obterUsuarios livro b
   where
     livro' = snd a
     usuario' = fst a





module Mercado where 

type Nome = String 
type Preco = Int 
type CodigoBarra = Int 
type BancoDeDados = [(CodigoBarra, Nome, Preco)] 

bd :: BancoDeDados 
bd = [(1001, "Refrigerante", 450), 
      (1002, "Leite", 320), 
      (1003, "Biscoito", 200),
      (1004, "Suco", 989),
      (1005, "Arroz", 345),
      (1006, "Feijao", 780)]


{-
 - RESOLUCAO DA ATIVIDADE DE REVISAO!!!
 -}


-- Questao 01
buscarBDaux :: CodigoBarra -> BancoDeDados -> (Nome, Preco)
buscarBDaux _ [] = error "Produto nao cadastrado!!!"
buscarBDaux codigo ((codigoBarra, nome, preco) : resto) 
  | codigo == codigoBarra = (nome, preco)
  | otherwise = buscarBDaux codigo resto 

-- Questao 02
buscaBD :: CodigoBarra -> (Nome, Preco) 
buscaBD codigo = buscarBDaux codigo bd 

-- Questao 03
fazerConta :: [CodigoBarra] -> [(Nome, Preco)]
fazerConta [] = []
fazerConta (codigo:restante) = buscaBD codigo : fazerConta restante

-- Questao 04
dividir :: Int -> String 
dividir numero = show (div numero 100) ++ "." ++ show (mod numero 100)

-- Questao 05
repetir :: Int -> String -> String 
repetir 0 _ = ""
repetir numero caractere = caractere ++ repetir (numero -1) caractere

-- Questao 06 
tamanhoLinha :: Int 
tamanhoLinha = 30 

formatarLinha :: (Nome, Preco) -> String 
formatarLinha (nome, preco) = nome ++ repetir quantidade "." ++ dividir preco ++ "\n" 
  where
 -- a linha tem que ser de tamanho = 30, logo temos que desconsiderar o tamanho do nome e do preco  
  tamanhoNome = length nome  
  tamanhoPreco = length (dividir preco)
  quantidade = tamanhoLinha - (tamanhoNome + tamanhoPreco)

-- Questao 07 
formatarLinhas :: [(Nome, Preco)] -> String 
formatarLinhas [] = ""
formatarLinhas ((nome, preco) : restanteLista) = formatarLinha (nome, preco) ++ formatarLinhas restanteLista

-- Questao 08
calcularTotal :: [ (Nome, Preco) ] -> Int 
calcularTotal [] = 0 
calcularTotal ((nome, preco) : restanteLista) = preco + calcularTotal restanteLista

-- Questao 09 
formatarTotal :: Int -> String 
-- Um comportamento muito similar de uma funcao ja criada...
formatarTotal valorTotal = formatarLinha ("Total:", valorTotal)

{-
 - Caso queira desenvolver a funcao, segue abaixo comentada

formatarTotal valorTotal = "Total:" ++ repetir quantidade "." ++ dividir valorTotal ++ "\n" 
  where
  tamanhoPreco = length(dividir valorTotal) 
  quantidade = tamanhoLinha - (tamanhoPreco + length("Total:"))
-}

-- Questao 10 
formatarConta :: [(Nome, Preco)] -> String 
formatarConta lista = formatarLinhas lista ++ formatarTotal(calcularTotal lista)

imprimirConta :: [CodigoBarra] -> IO() 
imprimirConta lista = putStr (formatarConta (fazerConta lista))

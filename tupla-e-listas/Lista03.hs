module Lista03 where

import Data.Char (ord, chr)
import Data.Char (toUpper)
import Data.Char (isSpace)
import Data.Bifunctor (bimap)
import Data.List (intercalate)

--1. Elaborar uma função para calcular a distância entre dois pontos.

type Ponto = ( Float , Float)

distancia :: Ponto -> Ponto -> Float
distancia (x1,y1) (x2,y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

--2. Elaborar uma função que teste se um ponto esta ou não dentro de um círculo.
type Circulo = ( Centro , Raio )
type Centro = Ponto
type Raio = Float

dentroCirculo :: Circulo -> Ponto -> Bool  
dentroCirculo (c,r) (x,y) = (distancia c (x,y)) <= r

--3. Defina uma função minmax que calcula o mínimo e o máximo de três inteiros. Por exemplo: minmax 3 6 1 = (1,6)

minmax :: Int -> Int -> Int -> (Int,Int)
minmax x y z
    | x <= y && x <= z = (x,y)
    | y <= x && y <= z = (y,x)
    | otherwise = (z,y)

--4. Defina uma função maxocorre que calcula o máximo de três números juntamente com o número de ocorrências do máximo. 
-- Por exemplo:maxocorre 3 1 3 = (3,2)

maxocorre :: Int -> Int -> Int -> (Int,Int)
maxocorre x y z
    | x >= y && x >= z = (x,1)
    | y >= x && y >= z = (y,1)
    | otherwise = (z,1)

-- 5. Elaborar uma função para calcular o tamanho de uma lista de inteiros.

tamanhoLista :: [Int] -> Int
tamanhoLista [] = 0
tamanhoLista (x:xs) = 1 + tamanhoLista xs

--6. Elaborar uma função para somar os elementos de uma lista de inteiros.

somarLista :: [Int] -> Int
somarLista [] = 0
somarLista (x:xs) = x + somarLista xs

-- 7. Elaborar uma função dobrar os elementos de uma lista de inteiros.
dobrarLista:: [Int] -> [Int]
dobrarLista [] = []
dobrarLista (x:xs) = 2*x : dobrarLista xs

-- 8. Elaborar uma função para vericar se uma elemento é membro de uma lista de inteiros.

pertence :: Int -> [Int] -> Bool
pertence _ [] = False
pertence x (y:ys)
    | x == y = True
    | otherwise = pertence x ys

-- 9. Elaborar uma função para ordenar uma lista de inteiros utilizando o método Insert sort

ins :: Int -> [Int] -> [Int]
ins a [] = [a]
ins a (b:y)
   | a <= b = a : (b:y)
   | otherwise = b : ins a y

ordenarLista :: [Int] -> [Int]
ordenarLista [] = []
ordenarLista (x:xs) = ins x (ordenarLista xs)

--10. Elaborar uma função para ordenar uma lista de inteiros utilizando o métodos Selection sort

selectionSort:: [Int] -> [Int]
selectionSort [] = []
selectionSort (x:xs) = minimum (x:xs) : selectionSort (remove (minimum (x:xs)) (x:xs))

remove :: Int -> [Int] -> [Int]
remove _ [] = []
remove x (y:ys)
    | x == y = ys
    | otherwise = y : remove x ys

--11. Elaborar uma função para ordenar uma lista de inteiros utilizando o método MergeSort

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort (take (length xs `div` 2) xs)) (mergeSort (drop (length xs `div` 2) xs))

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

--12. Elaborar uma função para ordenar uma lista de inteiros utilizando o método QuickSort

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = quickSort [y | y <- xs, y < x] ++ [x] ++ quickSort [y | y <- xs, y >= x]

--13. Elaborar uma função para contar o número de vogais de uma string.
contagem:: String -> Int
contagem str = length [x | x <- str, elem x "aeiouAEIOU"]

--14. Elaborar uma função que receba uma frase e devolva a quantidade de palavras na frase.

quantidadePalavras :: String -> Int
quantidadePalavras str = length (words str)

--14. Elaborar uma função que receba uma frase e devolva a quantidade de palavras na frase.

quantidadePalavras2 :: String -> Int
quantidadePalavras2 str = contarPalavras str False 0
  where
    contarPalavras [] _ count = count
    contarPalavras (x:xs) inWord count
      | x == ' '  = contarPalavras xs False count
      | inWord    = contarPalavras xs True count
      | otherwise = contarPalavras xs True (count + 1)

--15. Elaborar uma função para criptografar uma mensagem utilizando acifra de César.

criptografar :: String -> Int -> String
criptografar str n = [chr ((ord c - ord 'a' + n) `mod` 26 + ord 'a') | c <- str]

--16. Elaborar uma função para inverter uma string

inverter:: String -> String
inverter [] = []
inverter (x:xs) = inverter xs ++ [x]

--17. Elaborar uma função que receba uma frase com letras minúsculas e
--converta a primeira letra de cada palavra da frase para maiúscula

primeiraLetraMaiuscula :: String -> String
primeiraLetraMaiuscula [] = []
primeiraLetraMaiuscula (x:xs)
  | isSpace x = x : primeiraLetraMaiuscula xs  -- Se for um espaço, mantém e segue
  | otherwise = toUpper x : primeiraLetra xs  -- Capitaliza a primeira letra e processa o restante
  where
    primeiraLetra [] = []
    primeiraLetra (y:ys)
      | isSpace y = y : primeiraLetraMaiuscula ys 
      | otherwise = y : primeiraLetra ys 

--18. Elaborar uma função para implementar a busca binária em uma lista de inteiros.

buscaBinaria :: Int -> [Int] -> Bool
buscaBinaria _ [] = False
buscaBinaria x (y:ys)
    | x == y = True
    | x < y = buscaBinaria x ys
    | otherwise = False

--19. Elaborar uma função para computar o maior elemento de uma lista de inteiros.

maiorElemento :: [Int] -> Int
maiorElemento [x] = x
maiorElemento (x:xs) = max x (maiorElemento xs)


--20. Elaborar uma função para computar o maior e o menor elemento de uma lista de inteiros.

maiorMenorElemento :: [Int] -> (Int, Int)
maiorMenorElemento [] = error "Lista vazia"
maiorMenorElemento [x] = (x, x)
maiorMenorElemento (x:xs) = bimap (max x) (min x) (maiorMenorElemento xs)

--21. Elaborar uma função para contar o números de letras e palavras de uma frase

contarLetrasPalavras :: String -> (Int, Int)
contarLetrasPalavras str = (length [x | x <- str, isAlpha x], length (words str))
  where
    isAlpha x = elem x ['a'..'z'] || elem x ['A'..'Z']

--22. Elaborar uma função para vericar se uma string é uma palíndroma.

palindromo :: String -> Bool
palindromo str = str == inverter str

-- 23. Elaborar uma função que receba um nome completo de uma pessoa e
-- mostre os nomes intermediários entre o primeiro e o último sobrenome abreviados.

abreviarNome :: String -> String
abreviarNome nomeCompleto = unwords $ abreviar $ words nomeCompleto
  where
    abreviar [] = []
    abreviar [x] = [x]
    abreviar (x:xs) = x : abreviarIntermediarios xs
    abreviarIntermediarios [x] = [x]
    abreviarIntermediarios (x:xs) = (head x : ".") : abreviarIntermediarios xs

--24. Elaborar uma função que determine se um número é perfeito, ou seja, é igual a soma dos seus divisores (exceto o próprio).

ehPerfeito :: Int -> Bool
ehPerfeito x = sum [y | y <- [1..x-1], x `mod` y == 0] == x

--25. Elaborar uma função que intercala duas lista de inteiros ordenadas.

intercala :: [Int] -> [Int] -> [Int]
intercala [] ys = ys  
intercala xs [] = xs  
intercala (x:xs) (y:ys)
  | x <= y    = x : intercala xs (y:ys)  
  | otherwise = y : intercala (x:xs) ys  

-- 26.Uma função que devolve True se uma dada lista tem mais que 10 elementos, False caso contrário.

dezElementos :: [Int] -> Bool
dezElementos xs = length xs > 10

--27. Utilizando recursão por cauda (tail recursion) criar uma função para somar os elementos de uma lista de inteiros.

somaListaTail :: [Int] -> Int
somaListaTail lista = somaAux lista 0 

-- Função auxiliar com acumulador
somaAux :: [Int] -> Int -> Int
somaAux [] acc = acc  
somaAux (x:xs) acc = somaAux xs (acc + x)


-- 29. Baseado no banco de dados abaixo, elabore as seguintes funções:
type Matricula = Int
type Nome = String
type Titulacao = String
type Sexo = Char

banco :: Int -> (Matricula, Nome, Titulacao, Sexo)
banco matricula
  | matricula == 1 = (1, "Roque", "Doutor", 'M')
  | matricula == 2 = (2, "Alzira", "Doutor", 'F')
  | matricula == 3 = (3, "Helio", "Doutor", 'M')
  | matricula == 4 = (4, "Maisa", "Doutor", 'F')
  | matricula == 5 = (5, "Carlos", "Mestre", 'M')
  | matricula == 6 = (6, "Rita", "Mestre", 'F')
  | otherwise      = (0, "", "", ' ')

-- (a) Obter o número de doutores:

numDoutores :: Int -> Int
numDoutores 0 = 0
numDoutores n
  | titulacao == "Doutor" = 1 + numDoutores (n - 1)
  | otherwise = numDoutores (n - 1)
  where (_, _, titulacao, _) = banco n

-- (b) Obter o número de mulheres:

numMulheres :: Int -> Int
numMulheres 0 = 0
numMulheres n
  | sexo == 'F' = 1 + numMulheres (n - 1)
  | otherwise = numMulheres (n - 1)
  where (_, _, _, sexo) = banco n

-- (c) Obter o número de mulheres que são mestres:

numMulheresMestres :: Int -> Int
numMulheresMestres 0 = 0
numMulheresMestres n
  | sexo == 'F' && titulacao == "Mestre" = 1 + numMulheresMestres (n - 1)
  | otherwise = numMulheresMestres (n - 1)
  where (_, _, titulacao, sexo) = banco n

-- (d) Obter o nome de todos os doutores:

nomesDoutores :: Int -> [Nome]
nomesDoutores 0 = []
nomesDoutores n
  | titulacao == "Doutor" = nome : nomesDoutores (n - 1)
  | otherwise = nomesDoutores (n - 1)
  where (_, nome, titulacao, _) = banco n

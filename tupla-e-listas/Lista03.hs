module Lista03 where

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
dobrarLista (x:xs) = 2*x : dobrLista xs

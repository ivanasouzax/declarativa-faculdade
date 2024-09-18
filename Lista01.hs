module Aula01 where

import Data.Char

-- Exercício 1

-- 1. Elaborar uma função para calcular a área de um triângulo. 
type Base = Float
type Altura = Float
type Area = Float

area :: Base -> Altura -> Area
area b h =  (b*h) / 2.0

-- 2. Elaborar uma função que recebe três inteiros e retorna o menor.
menor :: Int -> Int -> Int -> Int
menor x y z
    | x <= y && x <= z = x
    | y <= x && y <= z = y
    | otherwise = z

-- 3. Elaborar uma função que recebe quatro inteiros e verificar se todos são iguais. 
todosIguais :: Int -> Int -> Int -> Int -> Bool
todosIguais a b c d = (a == b) && (b == c) && (c == d)

-- 4.Elaborar uma função que receba três inteiros e verique se os três são diferentes.
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c = (a /= b) && (b /= c) && (a /= c)

-- 5. Elaborar uma função que receba quatro números e devolva a média ponderada, sabendo-se que os pesos são respectivamente 1,2,3 e 4.
mediaPonderada :: Float -> Float -> Float -> Float -> Float
mediaPonderada a b c d = (a + 2*b + 3*c + 4*d) / 10

--6.Elaborar uma função que calcule do desconto de 20% do preço de um produto
desconto :: Float -> Float
desconto preco = preco - (preco * 0.2)

-- 7. Elaborar uma função para calcular o XOR (ou exclusivo). 
xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

-- 8. Elaborar uma função para entrar com a razão de uma progressão aritmética e o valor do primeiro termo. Devolver o décimo termo da série. 
decimoTermo :: Float -> Float -> Float
decimoTermo x a1 = a1 + 9*x

-- 9. Elaborar uma função para receber um número e informar se ele é posi tivo, negativo ou nulo. 
testaNum :: Int -> String
testaNum x
    | x < 0 = "Numero negativo"
    | x > 0 = "Numero positivo"
    | x == 0 = "Numero nulo"
    | otherwise = "Numero desconhecido"

-- 10. Elaborar uma função para calcular o NAND (negação do AND). 
nand :: Bool -> Bool -> Bool
nand a b = not (a && b)

-- 11. Elaborar uma função para veri car se um caractere é um digito
caractere :: Char -> Bool
caractere c
    | c >= '0' && c <= '9' = True
    | otherwise = False

-- 12. Elaborar uma função para converter uma letra maiúscula para minús cula. 
minusculo c
    | c >= 'A' && c <= 'Z' = toLower c
    | otherwise = c

-- 13. Elaborar uma função para converter uma letra minúscula para maiús cula. 
maisculo :: Char -> Char
maisculo c
    | c >= 'a' && c <= 'z' = toUpper c
    | otherwise = c

-- 14. Elaborar uma função para somar os algarismos de um número inteiro N. Considere que N >= 0eN < 1000. 
somaAlgarismos :: Int -> Int
somaAlgarismos n
    | n >= 0 && n < 1000 = (n `mod` 10) + somaAlgarismos (n `div` 10)
    | otherwise = n

-- 15. Elaborar uma função para calcular a média aritmética das três notas. 
mediaAritmetica :: Float -> Float -> Float -> Float
mediaAritmetica x y z = (x + y + z) / 3

-- 16. Elaborar uma função para receber três números e verificar se eles podem ou não ser lados de um triângulo
triangulo :: Float -> Float -> Float -> Bool
triangulo a b c = (a + b > c) && (a + c > b) && (b + c > a)

-- 17. Elaborar uma função para receber três números e devolver o tipo de triângulo (segundo seus ângulos) 
tipoTriangulo :: Float -> Float -> Float -> String
tipoTriangulo a b c
    | a == 90 || b == 90 || c == 90 = "Triangulo retangulo"
    | a > 90 || b > 90 || c > 90 = "Triangulo obtusangulo"
    | a < 90 && b < 90 && c < 90 = "Triangulo acutangulo"
    | otherwise = "Triangulo desconhecido"

-- 18. Elaborar uma funcão para calcular a soma de um progressão aritmética finita. 
somaPA :: Float -> Float -> Float -> Float
somaPA a1 an n = (a1 + an) * n / 2

-- 19. Elaborar uma função para receber as coordenadas de um ponto e in formar a localização no plano cartesiano. Testar os 9 casos possíveis.
localizacao :: Float -> Float -> String
localizacao x y
    | x > 0 && y > 0 = "Primeiro quadrante"
    | x < 0 && y > 0 = "Segundo quadrante"
    | x < 0 && y < 0 = "Terceiro quadrante"
    | x > 0 && y < 0 = "Quarto quadrante"
    | x == 0 && y == 0 = "Origem"
    | x == 0 && y /= 0 = "Eixo Y"
    | x /= 0 && y == 0 = "Eixo X"
    | otherwise = "Localizacao desconhecida"

-- 20. Elaborar uma função para ler um número inteiro N entre 1 e 12 e devolver o nome do mês. 
mes :: Int -> String
mes x
    | x == 1 = "Janeiro"
    | x == 2 = "Fevereiro"
    | x == 3 = "Marco"
    | x == 4 = "Abril"
    | x == 5 = "Maio"
    | x == 6 = "Junho"
    | x == 7 = "Julho"
    | x == 8 = "Agosto"
    | x == 9 = "Setembro"
    | x == 10 = "Outubro"
    | x == 11 = "Novembro"
    | x == 12 = "Dezembro"
    | otherwise = "Mes desconhecido"

-- 21. Elaborar uma função que recebe um número e retorna verdadeiro se for par. 
verificaPar :: Int -> Bool
verificaPar x
     | mod x 2 == 0 = True
     | otherwise = False

-- 22. Elaborar uma função para retornar o número de raízes de uma equação do segundo grau. 
raizes :: Float -> Float -> Float -> Float
raizes a b c = (-b + sqrt(b^2 - 4*a*c)) / (2*a)

-- 23. Elaborar uma função que converte a temperatura de graus farenheit para centigrados. 
conversao :: Float -> Float 
conversao f = (f - 32) / 1.8

-- 24. Elaborar uma função para calcular o IMC (Índice de Massa Corporal) de uma pessoa. Pesquise a tabela de IMC na Internet. 
imc :: Float -> Float -> Float
imc p a = p / (a * a)

classificarIdade :: Int -> String
classificarIdade i
    | i >= 0 && i < 12 = "Criança"
    | i >= 12 && i < 65 = "Adulto"
    | i >= 65 = "Idoso"
    | otherwise = "Idade desconhecida"

-- 26. Escreva uma função classi carNota :: Int -> String que classi ca uma nota como "F"(0-49), "D"(50-59), "C"(60-69), "B"(70-79) ou "A"(80- 100). 
classificarNota :: Int -> String
classificarNota n
    | n >= 0 && n <= 49 = "F"
    | n >= 50 && n <= 59 = "D"
    | n >= 60 && n <= 69 = "C"
    | n >= 70 && n <= 79 = "B"
    | n >= 80 && n <= 100 = "A"
    | otherwise = "Nota desconhecida"

-- 27. Escreva uma função proximoCaractere :: Char -> Char que retorna o próximo caractere na sequência alfabética. Se o caractere for 'z' ou 'Z', retorne 'a' ou 'A', respectivamente. 
proximoCaractere :: Char -> Char
proximoCaractere c
    | c == 'z' || c == 'Z' = 'a'
    | otherwise = succ c

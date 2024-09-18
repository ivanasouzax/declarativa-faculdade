-- 1. Denfir uma função para somar todos os pares de 1 até N.
somaPar :: Int -> Int
somaPar limite
    | limite < 1 = 0
    | ehPar limite == True = limite + somaPar(limite - 1)
    | otherwise = somaPar(limite - 1) 
      where
      ehPar numero = if(mod numero 2 == 0) then True else False

--2. Denfir uma função para calcular x elevado a y. Considere x e y inteiros positivos.
elevado :: Int -> Int -> Int
elevado x 0 = 1 
elevado x y = x * elevado x (y -1 )

--3. Denfir uma função para somar todos os números em um intervalo entre A e B, inclusive.
somaValores :: Int -> Int -> Int
somaValores a b
  |a <= b = a + somaValores( a + 1) b
  |otherwise = 0

--4. Denfir uma função para somar os dígitos de um número inteiro positivo N.
somaDigitos :: Integer -> Integer
somaDigitos n
  |n < 0 = error "Numero deve ser positivo"
  |n < 10 = n
  |otherwise = (n `mod` 10) + somaDigitos(n `div` 10)


--5. Denfir uma função que, dado um valor inteiro positivo N, retorna a soma dos quadrados de todos os inteiros compreendidos entre 0 e N,inclusive.
somaQuadrados :: Integer -> Integer
somaQuadrados n
  |n < 0 = error "Numero deve ser positivo"
  |n == 0 = 0
  |otherwise = (n * n) + somaQuadrados(n - 1)

-- Função auxiliar para calcular o fatorial
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

-- 6. Definir uma função que, dado um valor inteiro positivo N, retorna a soma dos fatoriais de todos os inteiros entre 0 e N.
somaFatorial :: Integer -> Integer
somaFatorial 0 = fatorial 0
somaFatorial n = fatorial n + somaFatorial (n - 1)

-- 7. Escreva uma função recursiva approxPi :: Integer -> Double que calcule
-- uma aproximação de π somando os primeiros n termos da série de Leibniz

approxPi :: Integer -> Double
approxPi 0 = 0
approxPi n = (4 / (2 * n + 1)) - (4 / (2 * (n + 1))) + approxPi (n - 1)

-- 8. Denir uma função para somar dois números utilizando a função sucessor.
somaSucc :: Integer -> Integer -> Integer
somaSucc a b
  |b == 0 = a
  |otherwise = somaSucc (a + 1) (b - 1)

-- 9. Denfir uma função para multiplicar dois números utilizando a função soma.
somaMulti :: Integer -> Integer -> Integer
somaMulti x y 
   |y == 0 = 0
   |otherwise = x + somaMulti x (y - 1)

-- 10.Denfir uma função que determine quantas vezes um dígito K ocorre em um número natural N. Por exemplo, o dígito 2 ocorre 3 vezes em
contarDigitoRec :: Int -> Int -> Int
contarDigitoRec _ 0 = 0 
contarDigitoRec k n 
   | mod n 10 == k = 1 + contarDigitoRec k (div n 10) 
   | otherwise = contarDigitoRec k (div n 10)

-- 11. Denfir uma função para converter um número decimal em binário.
binario:: Integer -> [Integer]
binario 0 = [0]
binario x 
   |x < 0 = error "Número deve ser positivo"
   |x == 1 = [1]
   |otherwise = binario(x `div` 2) ++ [x `mod` 2]

-- 12. Denfir uma função para concatenar uma palavra N vezes.
concatenar :: String -> Integer -> String
concatenar _ 0 = ""
concatenar palavra n = palavra ++ concatenar palavra (n - 1)

-- 13. O que é recursão de cauda (tail recursion)?
-- Recursão de cauda (ou tail recursion) é um tipo especial de recursão onde a chamada recursiva é a última operação a ser realizada por uma função. Isso significa que a função retorna diretamente o resultado da chamada recursiva, sem realizar mais cálculos ou operações após essa chamada.
-- Chamada recursiva como última operação: A função faz a chamada recursiva e não precisa "lembrar" de fazer algo após essa chamada, pois não há operações pendentes.
-- Eficiência: Linguagens como Haskell, Scala, e muitas outras, otimizam a recursão de cauda. Elas transformam a recursão de cauda em uma iteração interna, evitando o crescimento da pilha de chamadas e prevenindo estouros de pilha (stack overflow).

--14. Denfir uma função para receber um número N e devolver a tabuada de N.
tabuada :: Int -> Int -> IO ()
tabuada _ 11 = return ()  -- Caso base: quando chega a 11, para
tabuada n i = do
    print (n, "x", i, "=", n * i)  -- Imprime o resultado da multiplicação
    tabuada n (i + 1)  -- Chama a função recursivamente para o próximo valor de i

-- Função principal para começar a tabuada do número N
tabuadaN :: Int -> IO ()
tabuadaN n = tabuada n 1  -- Inicia a tabuada a partir do número 1                                                             

-- 15. Defina uma função fibonacci que receba um inteiro n e retorne o n- ésimo número da sequência de Fibonacci. 
--Defina a função fibonacci utilizando recursão de cauda. Compare o tempo de execução.
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Fibonacci usando recursão de cauda
fibonacciTail :: Int -> Int
fibonacciTail n = fibAux n 0 1
  where
    fibAux 0 a _ = a
    fibAux n a b = fibAux (n - 1) b (a + b)

--16. Calcular o valor de e com n termos 
-- Função recursiva para calcular o fatorial
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

-- 17. Função recursiva para calcular o valor de e usando n termos
calcularE :: Int -> Double
calcularE 0 = 1  -- Caso base: o primeiro termo da série é 1
calcularE n = 1 / fromIntegral (fatorial n) + calcularE (n - 1)


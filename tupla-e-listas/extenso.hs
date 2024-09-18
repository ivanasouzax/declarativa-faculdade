-- Lista de unidades por extenso
unidades :: [String]
unidades = ["zero", "um", "dois", "três", "quatro", "cinco", "seis", "sete", "oito", "nove",
            "dez", "onze", "doze", "treze", "catorze", "quinze", "dezesseis", "dezessete", "dezoito", "dezenove"]

-- Lista de dezenas por extenso
dezenas :: [String]
dezenas = ["", "", "vinte", "trinta", "quarenta", "cinquenta", "sessenta", "setenta", "oitenta", "noventa"]

-- Lista de centenas por extenso
centenas :: [String]
centenas = ["", "cento", "duzentos", "trezentos", "quatrocentos", "quinhentos", "seiscentos", "setecentos", "oitocentos", "novecentos"]

-- Função para converter números de 1 a 999
numeroPorExtenso :: Int -> String
numeroPorExtenso n
    | n == 1000 = "mil"
    | n == 100 = "cem"
    | n < 20 = unidades !! n  -- Trata números de 0 a 19 diretamente
    | n < 100 = dezenaPorExtenso n  -- Trata números de 20 a 99
    | otherwise = centenaPorExtenso n  -- Trata números de 100 a 999

-- Função auxiliar para dezenas
dezenaPorExtenso :: Int -> String
dezenaPorExtenso n
    | n < 20 = unidades !! n  -- Se o número estiver entre 10 e 19, é um caso especial
    | unidade == 0 = dezenas !! dezena  -- Se a unidade for 0, apenas a dezena é usada
    | otherwise = dezenas !! dezena ++ " e " ++ unidades !! unidade  -- Combina dezena com unidade
    where
        dezena = n `div` 10  -- Extrai a dezena
        unidade = n `mod` 10  -- Extrai a unidade

-- Função auxiliar para centenas
centenaPorExtenso :: Int -> String
centenaPorExtenso n
    | n == 100 = "cem"  -- Caso especial para "cem"
    | resto == 0 = centenas !! centena  -- Se não houver resto, apenas a centena é usada
    | otherwise = centenas !! centena ++ " e " ++ numeroPorExtenso resto  -- Combina a centena com o restante do número
    where
        centena = n `div` 100  -- Extrai a centena
        resto = n `mod` 100  -- Extrai o resto

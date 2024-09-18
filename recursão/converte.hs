
import Data.Char

-- converte "Ricardo Souza Silva"
-- "rss@uesb.edu.br"

-- getWord "Ricardo Souza Silva" = "Ricardo"
getWord :: String -> String
getWord [] = []
getWord (a:b) 
    | a == ' ' = []
    | otherwise = a : getWord b

-- dropWord "Ricardo Souza Silva" = "Souza Silva"
dropWord :: String -> String
dropWord [] = []
dropWord (a:b)
    | a == ' ' = b
    | otherwise = dropWord b

-- splitWord "Ricardo Souza Silva" = ["Ricardo","Souza","Silva"]
splitWord :: String -> [String]
splitWord [] = []
splitWord str = getWord str  :  splitWord (dropWord str)


-- pegarInicial "Ricardo" = "r"
pegarInicial :: String -> String
pegarInicial (a:b) = [toLower a]

-- transformar ["Ricardo","Souza","Silva"] = "rss"
transformar :: [String] -> String
transformar [] = []
transformar (a:b) = pegarInicial a ++ transformar b

converte :: String -> String
converte str = transformar (splitWord(removerPalavras str)) ++ "@uesb.edu.br"

-- Converter uma lista de nomes em uma lista de e-mails
-- converte ["Ricardo Souza Silva", "Ivana Souza Santos"] = ["rss@uesb.edu.br","iss@uesb.edu.br"]
converte2 :: [String] -> [String]
converte2 [] = []
converte2 (a:b) = converte a : converte2 b

-- Remover palavras "do", "dos", "de", "da", "das", porém existe um erro porque remover também o ultimo sobrenome
removerPalavras :: String -> String
removerPalavras = removerPalavrasAux "" -- Função auxiliar começa com palavra vazia

removerPalavrasAux :: String -> String -> String
removerPalavrasAux _ [] = []  
removerPalavrasAux palavra (a:b)
    | a == ' ' = processarPalavra palavra ++ " " ++ removerPalavrasAux "" b
    | otherwise = removerPalavrasAux (palavra ++ [a]) b

-- Função que verifica se a palavra deve ser removida
processarPalavra :: String -> String
processarPalavra x
    | x`elem` ["do", "dos", "de", "da", "das"] = ""  -- Remove se for uma das palavras
    | otherwise = x  -- Mantém a palavra caso não seja uma das indesejadas

-- Verificar o tamanho dos espaços
-- "Ricardo    Souza     Silva"  = "Ricardo Souza Silva"
removerEspacosDuplicados :: String -> String
removerEspacosDuplicados [] = []
removerEspacosDuplicados [x] = [x]  -- Caso de apenas um caractere
removerEspacosDuplicados (a:b:c)
    | a == ' ' && b == ' ' = removerEspacosDuplicados (b:c)  -- Se dois espaços consecutivos, ignora um
    | otherwise = a : removerEspacosDuplicados (b:c)

-- Verificar o tamanho do nome e aceita apenas com sobrenome
-- "Ricardo" = nome invalido
-- "Ricardo Souza Silva" = nome valido
verificarNome :: String -> String
verificarNome [] = "nome invalido"
verificarNome (a:b)
    | a == ' ' = b
    | otherwise = verificarNome b

-- Verificar pessoas com as mesmas iniciais
-- rss@uesb.edu.br = rss2@uesb.edu.br
-- verificarMesmasIniciais :: String -> String
-- verificarMesmasIniciais [] = []
-- verificarMesmasIniciais (a:b)
--     |


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
transformar (palavra:restante)
  | minuscula palavra == "do" ||  minuscula palavra == "dos" || minuscula palavra == "de" || 
      minuscula palavra == "da" || minuscula palavra == "das" = transformar restante 
  | otherwise = pegarInicial palavra ++ transformar restante 
  where 
    minuscula [] = [] 
    minuscula (caractere:palavra) = toLower(caractere) : minuscula palavra 

converte :: String -> String
converte str = transformar (splitWord(removerEspacosDuplicados str)) ++ "@uesb.edu.br"

-- Converter uma lista de nomes em uma lista de e-mails
-- converte ["Ricardo Souza Silva", "Ivana Souza Santos"] = ["rss@uesb.edu.br","iss@uesb.edu.br"]
converte2 :: [String] -> [String]
converte2 [] = []
converte2 (a:b) = converte a : converte2 b

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

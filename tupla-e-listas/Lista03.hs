--1. Elaborar uma função para calcular a distância entre dois pontos.

type Ponto = ( Float , Float)

distancia :: Ponto -> Ponto -> Float
distancia (x1,y1) (x2,y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

--2. Elaborar uma função que teste se um ponto esta ou não dentro de um círculo.
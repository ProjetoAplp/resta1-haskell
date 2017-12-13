module Constantes(cima, baixo, esquerda, direita, incrementoAdjacente, incrementoDestino, sequenciaDeVitoria) where

cima :: Int
cima = 0

baixo :: Int
baixo = 1

esquerda :: Int
esquerda = 2

direita :: Int
direita = 3

incrementoAdjacente :: Int
incrementoAdjacente = 1

incrementoDestino :: Int
incrementoDestino = 2

sequenciaDeVitoria :: [[Int]]
sequenciaDeVitoria = [[2,4,1], [5,4,0], [4,6,2], [4,3,3], [2,3,1], [5,6,2], [3,1,3], [7,5,0], [3,4,2], [4,5,1], [5,1,0], [7,3,3], [3,1,3], [7,5,0], [4,3,0], [5,4,3], [3,6,2], [1,5,1], [5,7,2], [5,2,3], [5,4,3], [3,7,1], [5,7,2], [6,3,3], [6,5,0], [4,5,0], [1,3,3], [1,5,1], [3,5,2], [2,3,1], [4,2,3]]
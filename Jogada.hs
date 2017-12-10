module Jogada(realizaJogada) where
import Constantes
import Validacao
import Data.Matrix


realizaJogada :: Int -> Int -> Int -> Matrix Char -> Matrix Char
realizaJogada linha coluna direcao matriz =
    setElem '0' (definePosicaoAdjacente linha coluna direcao) (setElem '1' (definePosicaoDestino linha coluna direcao) (setElem '0' (definePosicaoOrigem linha coluna) matriz))

definePosicaoOrigem :: Int -> Int -> (Int, Int)
definePosicaoOrigem linha coluna = (linha, coluna)

definePosicaoAdjacente :: Int -> Int -> Int -> (Int, Int)
definePosicaoAdjacente linha coluna direcao = definePosicao linha coluna direcao incrementoAdjacente

definePosicaoDestino :: Int -> Int -> Int -> (Int, Int)
definePosicaoDestino linha coluna direcao = definePosicao linha coluna direcao incrementoDestino

definePosicao :: Int -> Int -> Int -> Int -> (Int, Int)
definePosicao linha coluna 0 incremento = (linha-incremento, coluna) 
definePosicao linha coluna 1 incremento = (linha+incremento, coluna) 
definePosicao linha coluna 2 incremento = (linha, coluna-incremento) 
definePosicao linha coluna 3 incremento = (linha, coluna+incremento) 
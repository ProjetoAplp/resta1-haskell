module Jogada(realizaJogada, realizaJogadaAutomatica) where
import Constantes
import Validacao
import Data.Matrix


realizaJogada :: Int -> Int -> Int -> Matrix Char -> Matrix Char
realizaJogada linha coluna direcao matriz =
    setElem '0' (definePosicaoAdjacente linha coluna direcao) (setElem '1' (definePosicaoDestino linha coluna direcao) (setElem '0' (definePosicaoOrigem linha coluna) matriz))

realizaJogadaAutomatica :: Matrix Char -> Matrix Char
realizaJogadaAutomatica tabuleiro = (realizaJogada ((selecionaJogada 0 0 0 tabuleiro) !! 0) ((selecionaJogada 0 0 0 tabuleiro) !! 1) ((selecionaJogada 0 0 0 tabuleiro) !! 2) tabuleiro)

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

{-
- Seleciona uma jogada automática percorrendo todo o tabuleiro.
-}
selecionaJogada :: Int -> Int -> Int -> Matrix Char -> [Int]
selecionaJogada linha coluna direcao tabuleiro
    | (validaJogada linha coluna direcao tabuleiro) = [linha, coluna, direcao]
    | ((linha == 7) && (coluna == 7) && (direcao == 3)) = [0, 0, 1]
    | ((coluna == 7) && (direcao == 3)) = selecionaJogada (linha + 1) 0 0 tabuleiro
    | (direcao == 3) = selecionaJogada linha (coluna + 1) 0 tabuleiro
    | otherwise = selecionaJogada linha coluna (direcao + 1) tabuleiro
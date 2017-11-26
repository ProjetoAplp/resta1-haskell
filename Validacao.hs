module Validacao(isJogadaValida) where

import Constantes
import Data.Matrix

isJogadaValida :: Int -> Int -> Int -> Matrix Char -> Bool
isJogadaValida linha coluna direcao matriz = 
    (validaLimiteTabuleiro linha coluna) &&
    (validaMovimento linha coluna direcao) &&
    (validaOrigem linha coluna matriz) &&
    (validaAdjacente linha coluna direcao matriz) &&
    (validaDestino linha coluna direcao matriz)

validaLimiteTabuleiro :: Int -> Int -> Bool
validaLimiteTabuleiro linha coluna
    | (linha < 1) || (linha > 7) = False -- nao esta no intervalo horizontal do tabuleiro
    | (coluna < 1) || (coluna > 7) = False -- nao esta no intervalo vertical do tabuleiro
    | otherwise = True

{-
- Valida se o movimento esta nos limites da matriz
-}
validaMovimento :: Int -> Int -> Int -> Bool
validaMovimento linha coluna 0 = linha >= 3 --cima
validaMovimento linha coluna 1 = linha <= 5 --baixo
validaMovimento linha coluna 2 = coluna >= 3 --esquerda
validaMovimento linha coluna 3 = coluna <= 5 --direita
validaMovimento linha coluna _ = False

{-
- Valida se existe peca na casa de origem
-}
validaOrigem :: Int -> Int -> Matrix Char -> Bool
validaOrigem linha coluna matriz = ((getElem linha coluna matriz) == '1')

{-
- Valida se existe peca na casa de origem
-}
validaAdjacente :: Int -> Int -> Int -> Matrix Char -> Bool
validaAdjacente linha coluna 0 matriz = ((getElem (linha - 1) coluna matriz) == '1') --cima
validaAdjacente linha coluna 1 matriz = ((getElem (linha + 1) coluna matriz) == '1') --baixo
validaAdjacente linha coluna 2 matriz = ((getElem linha (coluna - 1) matriz) == '1') --esquerda
validaAdjacente linha coluna 3 matriz = ((getElem linha (coluna + 1) matriz) == '1') --direita
validaAdjacente linha coluna _ matriz = False

{-
- Valida se existe peca na casa de origem
-}
validaDestino :: Int -> Int -> Int -> Matrix Char -> Bool
validaDestino linha coluna 0 matriz = ((getElem (linha - 2) coluna matriz) == '0') --cima
validaDestino linha coluna 1 matriz = ((getElem (linha + 2) coluna matriz) == '0') --baixo
validaDestino linha coluna 2 matriz = ((getElem linha (coluna - 2) matriz) == '0') --esquerda
validaDestino linha coluna 3 matriz = ((getElem linha (coluna + 2) matriz) == '0') --direita
validaDestino linha coluna _ matriz = False
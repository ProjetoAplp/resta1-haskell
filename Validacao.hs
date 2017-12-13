module Validacao(isJogadaValida, existeJogada, checaVitoria, validaEntradaJogada, validaJogada) where

import Constantes
import Data.Matrix
import qualified Data.List as List

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

{-
- Dado um tabuleiro representado como Matrix, 
- verifica se ainda existe alguma jogada válida a ser realizada
-}
existeJogada :: Matrix Char -> Bool
existeJogada tabuleiro =
    let tabuleiroRotacionado = rotacionaTabuleiroDireita tabuleiro in
    checaJogadaLinhas (toLists tabuleiro) ||
    checaJogadaLinhas (toLists tabuleiroRotacionado)

{-
- Dada a representação do tabuleiro como uma lista de listas, 
- verifica se existe jogada válida linha a linha
-}
checaJogadaLinhas :: [[Char]] -> Bool
checaJogadaLinhas [] = False
checaJogadaLinhas (xs:xss) = (checaJogadaLinha xs) || (checaJogadaLinhas xss) 


{-
- Verifica se há alguma jogada válida na lista passada, 
- que representa uma linha do tabuleiro
-}
checaJogadaLinha :: [Char] -> Bool
checaJogadaLinha linha = 
    (List.isInfixOf ['1','1','0'] linha) || (List.isInfixOf ['0','1','1'] linha)

{-
Rotaciona o tabuleiro para a direita, ou seja:
a b c       g d a
d e f   =>  h e b
g h i       i f c
-}
rotacionaTabuleiroDireita :: Matrix Char -> Matrix Char
rotacionaTabuleiroDireita tabuleiro = 
    transpose (fromLists (List.reverse (toLists tabuleiro)))

{-Dado um tabuleiro sem jogadas possíveis, verifica se é um estado de vitória-}
checaVitoria :: Matrix Char -> Bool
checaVitoria tabuleiro = 
    let listaTabuleiro = toList tabuleiro in
    (length [a | a <- listaTabuleiro, a == '1']) == 1

{-
- Valida se uma entrada de jogada está nos intervalos corretos do tabuleiro.
-}
validaEntradaJogada :: Int -> Int -> Int -> Bool
validaEntradaJogada linha coluna direcao = 
    (linha >= 1) && (linha <= 7) &&
    (coluna >= 1) && (coluna <= 7) &&
    (direcao >= 0) && (direcao <= 3)
    
{-
- Valida as entradas da jogada.
-}
validaJogada :: Int -> Int -> Int -> Matrix Char -> Bool
validaJogada linha coluna direcao tabuleiro = ((validaEntradaJogada linha coluna direcao) && 
                                        (isJogadaValida linha coluna direcao tabuleiro))
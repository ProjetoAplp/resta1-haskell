module Tabuleiro(exibirTabuleiro, selecionaTabuleiro, tabuleiroIngles, tabuleiroEuropeu) where
import Data.Matrix

{-
-- Constroi o tabuleiro do tipo europeu
-}
tabuleiroEuropeu :: Matrix Char
tabuleiroEuropeu = fromLists
    [[' ',' ','1','1','1',' ',' '],
     [' ','1','1','1','1','1',' '],
     ['1','1','1','1','1','1','1'],
     ['1','1','1','0','1','1','1'],
     ['1','1','1','1','1','1','1'],
     [' ','1','1','1','1','1',' '],
     [' ',' ','1','1','1',' ',' ']]

{-
-- Constroi o tabuleiro do tipo ingles
-}
tabuleiroIngles :: Matrix Char
tabuleiroIngles = fromLists
    [[' ',' ','1','1','1',' ',' '],
     [' ',' ','1','1','1',' ',' '],
     ['1','1','1','1','1','1','1'],
     ['1','1','1','0','1','1','1'],
     ['1','1','1','1','1','1','1'],
     [' ',' ','1','1','1',' ',' '],
     [' ',' ','1','1','1',' ',' ']]

{-
- Seleciona o tabuleiro conforme opção do usuário
-}
selecionaTabuleiro :: IO (Matrix Char)
selecionaTabuleiro = do
    putStrLn "Selecione o tabuleiro: (0 - Ingles / 1 - Europeu)"
    op <- getLine
    if (op == "0") then return (tabuleiroIngles)
    else if (op == "1") then  return (tabuleiroEuropeu)
    else selecionaTabuleiro
{-
- Converte a matriz em uma string para ser impressa na saída.
-}
concatenar :: Int -> Int -> Matrix Char -> String
concatenar linha coluna tabuleiro 
    | ((linha == 1) && (coluna == 1)) = [(getElem 1 1 tabuleiro)] ++ " " ++ (concatenar 1 2 tabuleiro)
    | ((linha == 8) && (coluna == 8)) = [(getElem 8 8 tabuleiro)]
    | (coluna == 8) = [(getElem linha coluna tabuleiro)] ++ "\n" ++ (concatenar (linha + 1) 1 tabuleiro)
    | otherwise = [(getElem linha coluna tabuleiro)] ++ " " ++ (concatenar linha (coluna + 1) tabuleiro)


{-
-- Exibe o tabuleiro para o usuário
-}
exibirTabuleiro :: Matrix Char -> IO ()
exibirTabuleiro tabuleiro = putStrLn (concatenar 1 1 ( linha <-> ((coluna) <|> (tabuleiro))))
    where
        linha = fromLists [[' ', 'A','B','C','D','E','F', 'G']]
        coluna = fromLists [['1'],['2'],['3'],['4'],['5'],['6'],['7']]

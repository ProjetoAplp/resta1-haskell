module Tabuleiro(exibirTabuleiro, selecionaTabuleiro) where
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
-- Exibe o tabuleiro para o usuário
-}
exibirTabuleiro :: Matrix Char -> IO ()
exibirTabuleiro tabuleiro = print ( linha <-> ((coluna) <|> (tabuleiro)) )
    where
        linha = fromLists [[' ', 'A','B','C','D','E','F', 'G']]
        coluna = fromLists [['1'],['2'],['3'],['4'],['5'],['6'],['7']]

import Data.Matrix


{-
- Constantes
-}
cima = 0
baixo = 1
esquerda = 2
direita = 3
incrementoAdjacente = 1
incrementoDestino = 2


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
-- Exibe o tabuleiro para o usuário
-}
exibirTabuleiro :: Matrix Char -> IO ()
exibirTabuleiro tabuleiro = print ( linha <-> ((coluna) <|> (tabuleiro)) )
    where
        linha = fromLists [[' ', 'A','B','C','D','E','F', 'G']]
        coluna = fromLists [['1'],['2'],['3'],['4'],['5'],['6'],['7']]


{-
- Exibe as regras do jogo para o usuário
-}
exibirRegras :: IO ()
exibirRegras =
    do
        putStrLn ("RRRRRRRRRRRRRRRRR                                                 tttt                                   1111111")    
        putStrLn "R::::::::::::::::R                                             ttt:::t                                  1::::::1"    
        putStrLn "R::::::RRRRRR:::::R                                            t:::::t                                 1:::::::1"   
        putStrLn "RR:::::R     R:::::R                                           t:::::t                                 111:::::1"   
        putStrLn "  R::::R     R:::::R    eeeeeeeeeeee        ssssssssss   ttttttt:::::ttttttt      aaaaaaaaaaaaa           1::::1"   
        putStrLn "  R::::R     R:::::R  ee::::::::::::ee    ss::::::::::s  t:::::::::::::::::t      a::::::::::::a          1::::1"    
        putStrLn "  R::::RRRRRR:::::R  e::::::eeeee:::::eess:::::::::::::s t:::::::::::::::::t      aaaaaaaaa:::::a         1::::1"    
        putStrLn "  R:::::::::::::RR  e::::::e     e:::::es::::::ssss:::::stttttt:::::::tttttt               a::::a         1::::l"    
        putStrLn "  R::::RRRRRR:::::R e:::::::eeeee::::::e s:::::s  ssssss       t:::::t              aaaaaaa:::::a         1::::l"    
        putStrLn "  R::::R     R:::::Re:::::::::::::::::e    s::::::s            t:::::t            aa::::::::::::a         1::::l"    
        putStrLn "  R::::R     R:::::Re::::::eeeeeeeeeee        s::::::s         t:::::t           a::::aaaa::::::a         1::::l"    
        putStrLn "  R::::R     R:::::Re:::::::e           ssssss   s:::::s       t:::::t    tttttta::::a    a:::::a         1::::l"    
        putStrLn "RR:::::R     R:::::Re::::::::e          s:::::ssss::::::s      t::::::tttt:::::ta::::a    a:::::a      111::::::111" 
        putStrLn "R::::::R     R:::::R e::::::::eeeeeeee  s::::::::::::::s       tt::::::::::::::ta:::::aaaa::::::a      1::::::::::1" 
        putStrLn "R::::::R     R:::::R  ee:::::::::::::e   s:::::::::::ss          tt:::::::::::tt a::::::::::aa:::a     1::::::::::1" 
        putStrLn "RRRRRRRR     RRRRRRR    eeeeeeeeeeeeee    sssssssssss              ttttttttttt    aaaaaaaaaa  aaaa     111111111111" 
        putStrLn " "
        putStrLn "REGRAS:"  
        putStrLn "O objetivo eh deixar apenas uma peca no tabuleiro apos uma sequencia de movimentos validos."     
        putStrLn "O tabuleiro possui um espaco vazio no centro, representado pelo caractere '0', com um numero de pecas, representadas pelo caractere '1', que designam uma estrutura pre-definida." 
        putStrLn "O jogo apresenta duas formas de tabuleiro, a primeira com padrao ingles com 32 pecas e a outra o padrao europeu com 36 pecas." 
        putStrLn "Um movimento consiste em pegar uma peca e faze-la 'saltar' sobre outra peca, sempre na horizontal ou na vertical, terminando em um espaco vazio, representado pelo caractere '0', adjacente a peca 'saltada'. A peca que foi 'saltada' eh retirada do tabuleiro." 



leOpcaoTabuleiro :: IO (Matrix Char)
leOpcaoTabuleiro = do
    putStrLn "Selecione o tabuleiro: (0 - Ingles / 1 - Europeu)"
    op <- getLine
    if (op == "0") then return (tabuleiroIngles)
    else if (op == "1") then  return (tabuleiroEuropeu)
    else leOpcaoTabuleiro


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


main = do
    exibirRegras
    putStrLn " "
    tabuleiro <- leOpcaoTabuleiro
    exibirTabuleiro tabuleiro
    
    print (isJogadaValida 2 4 baixo tabuleiro)
    exibirTabuleiro (realizaJogada 2 4 baixo tabuleiro)
    
    print (isJogadaValida 6 4 cima tabuleiro)
    exibirTabuleiro (realizaJogada 6 4 cima tabuleiro)
    
    print (isJogadaValida 4 2 direita tabuleiro)
    exibirTabuleiro (realizaJogada 4 2 direita tabuleiro)
    
    print (isJogadaValida 4 6 esquerda tabuleiro)
    exibirTabuleiro (realizaJogada 4 6 esquerda tabuleiro)

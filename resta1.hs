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


main = do
    exibirRegras
    putStrLn " "
    tabuleiro <- leOpcaoTabuleiro
    exibirTabuleiro tabuleiro
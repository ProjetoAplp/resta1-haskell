import Data.Matrix
import Data.Char
import Tabuleiro
import Jogada
import Validacao
import Constantes

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

{-
- Mapeira a coluna indexada de A-G para seu valor numérico começando de 1
- Caso não esteja no intervalo especificado, é retornado o valor 8, 
- que não passa nas validações
-}
mapeiaLetraColuna :: [Char] -> Int
mapeiaLetraColuna "A" = 1
mapeiaLetraColuna "B" = 2
mapeiaLetraColuna "C" = 3
mapeiaLetraColuna "D" = 4
mapeiaLetraColuna "E" = 5
mapeiaLetraColuna "F" = 6
mapeiaLetraColuna "G" = 7
mapeiaLetraColuna  _  = 8

{-
- Valida se uma entrada de jogada está nos intervalos corretos do tabuleiro.
-}
validaEntradaJogada :: Int -> Int -> Int -> Bool
validaEntradaJogada linha coluna direcao = 
    (linha >= 1) && (linha <= 7) &&
    (coluna >= 1) && (coluna <= 7) &&
    (direcao >= 0) && (direcao <= 3)

{-
-Confere ao jogador a escolha de uma jogada automática
-}
isJogadaAutomatica :: IO(Bool)
isJogadaAutomatica = do
    putStrLn "Deseja realizar uma jogada automatica(s/n)?"
    opcao <- getLine
    if (opcao == "s") then return True
    else if (opcao == "n") then return False
    else isJogadaAutomatica

validaJogada :: Int -> Int -> Int -> Matrix Char -> Bool
validaJogada linha coluna direcao tabuleiro = ((validaEntradaJogada linha coluna direcao) && 
                                     (isJogadaValida linha coluna direcao tabuleiro))

selecionaJogada :: Int -> Int -> Int -> Matrix Char -> [Int]
selecionaJogada linha coluna direcao tabuleiro
    | (validaJogada linha coluna direcao tabuleiro) = [linha, coluna, direcao]
    | ((linha == 7) && (coluna == 7) && (direcao == 3)) = [0, 0, 1]
    | ((coluna == 7) && (direcao == 3)) = selecionaJogada (linha + 1) 0 0 tabuleiro
    | (direcao == 3) = selecionaJogada linha (coluna + 1) 0 tabuleiro
    | otherwise = selecionaJogada linha coluna (direcao + 1) tabuleiro

{-
- Loop principal do jogo, que é finalizado quando não existem mais jogadas a serem realizadas,
- a ultima ação do loop é verificar a vitória do jogador.
- TODO: Ao inserir Strings em entradas que esperam receber Int (linha e direção) é disparado um erro que finaliza a aplicação.
-}
gameLoop jogadaAutomatica tabuleiro
    | (jogadaAutomatica && (existeJogada tabuleiro)) =
        do
            exibirTabuleiro tabuleiro
            jogadaAutomatica <- isJogadaAutomatica
            putStrLn (show (selecionaJogada 0 0 0 tabuleiro))
            gameLoop jogadaAutomatica $ (realizaJogada ((selecionaJogada 0 0 0 tabuleiro) !! 0) ((selecionaJogada 0 0 0 tabuleiro) !! 1) ((selecionaJogada 0 0 0 tabuleiro) !! 2) tabuleiro)            

    | (not(jogadaAutomatica) && (existeJogada tabuleiro)) =
        do
            exibirTabuleiro tabuleiro
            putStrLn "Selecione a linha(1-7): "
            linhaInput <- getLine
            putStrLn "Selecione a coluna(A-G): "
            colunaInput <- getLine
            putStrLn "Selecione a direção(0 - Cima; 1 - Baixo; 2 - Esquerda; 3 - Direita): "
            direcaoInput <- getLine
            
            let linha = read linhaInput
                coluna = mapeiaLetraColuna (map toUpper colunaInput)
                direcao = read direcaoInput in 
            
                if (not (validaEntradaJogada linha coluna direcao)) 
                    then do
                        print "Entrada Invalida"
                        jogadaAutomatica <- isJogadaAutomatica
                        gameLoop jogadaAutomatica tabuleiro

                else if(not (isJogadaValida linha coluna direcao tabuleiro)) 
                    then do
                        print linha
                        print coluna
                        print direcao
                        print "Jogada Invalida"
                        jogadaAutomatica <- isJogadaAutomatica
                        gameLoop jogadaAutomatica tabuleiro

                else
                    do 
                        jogadaAutomatica <- isJogadaAutomatica
                        gameLoop jogadaAutomatica $ realizaJogada linha coluna direcao tabuleiro

    | otherwise = 
        do
            if (checaVitoria tabuleiro) then
                print "Parabens! Você venceu!"
            else
                print "Voce perdeu, tente novamente!"

{-
- Função main da aplicação, que inicializa o jogo e dispara o gameLoop
-}
main :: IO()
main = do
    exibirRegras
    putStrLn " "
    tabuleiro <- selecionaTabuleiro
    jogadaAutomatica <- isJogadaAutomatica
    gameLoop jogadaAutomatica tabuleiro

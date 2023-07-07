import Data.List (sort)

type Endereco = (String, Int, String)

data Pessoas  = Pessoa String Int  Endereco 
                deriving (Eq, Ord, Show)


imprimir :: [Pessoas] -> IO ()
imprimir [] = do putStrLn "\nRegistro VAZIO"
imprimir lista = do
  putStrLn "\n--------Registro--------"
  mapM_ print lista


adicionarPessoa::[Pessoas]->IO [Pessoas]
adicionarPessoa dados = do
    putStrLn "\n-------ADICIONAR-------"
    putStrLn "Digite o nome completo"
    nome <- getLine
    putStrLn "Digite a idade"
    idade <- getLine
    putStrLn "Digite o endereco" 
    putStrLn "Rua: "
    rua <- getLine
    putStrLn "Casa: "
    casa <- getLine
    putStrLn "Cidade: "
    cidade <- getLine

    return (Pessoa nome (read idade::Int) (rua,(read casa::Int),cidade) :dados) 

extrairNomes :: [Pessoas] ->[String]
extrairNomes [] = []
extrairNomes ((Pessoa nome _ _) : cauda) = nome : extrairNomes cauda

ordenar::[Pessoas]->[String]->[Pessoas]
ordenar x [] = x
ordenar ((Pessoa nome idade endereco):cauda) listaNomesOrdenados
    | nome == head listaNomesOrdenados = (Pessoa nome idade endereco) : ordenar (cauda) (tail listaNomesOrdenados)
    | otherwise = ordenar (cauda++[(Pessoa nome idade endereco)]) listaNomesOrdenados

registroOrdenado::[Pessoas]->[Pessoas]
registroOrdenado lista = ordenar lista (sort (extrairNomes lista))

encontrarPessoa::[Pessoas]->String->Pessoas
encontrarPessoa ((Pessoa nome idade endereco):cauda) nome2 
    | nome == nome2 = (Pessoa nome idade endereco)
    | otherwise = encontrarPessoa cauda nome2

removerPessoa::[Pessoas]->String->[Pessoas]
removerPessoa ((Pessoa nome idade endereco):cauda) nome2 
    | nome == nome2 = cauda
    | otherwise = (Pessoa nome idade endereco) : removerPessoa cauda nome2

populacaoCidade::[Pessoas]->String->Int
populacaoCidade [] _ = 0
populacaoCidade lista cidade1 = length [(Pessoa nome idade (rua, casa, cidade)) | (Pessoa nome idade (rua, casa, cidade))<-lista, cidade==cidade1]

mediaIdadePopulacao::[Pessoas]->Float
mediaIdadePopulacao [] = 0
mediaIdadePopulacao lista = fromIntegral (sum listaIdades) / fromIntegral qtdPessoas
  where
    listaIdades = [idade | (Pessoa nome idade (rua, casa, cidade))<-lista]
    qtdPessoas = length listaIdades
    

menu :: [Pessoas] -> IO ()
menu dados = do
  putStrLn "\n--------MENU--------"
  putStrLn "Digite 1 para cadastrar pessoa"
  putStrLn "Digite 2 para localizar pessoa"
  putStrLn "Digite 3 atualizar pessoa"
  putStrLn "Digite 4 para consultar total de pessoas em uma cidade"
  putStrLn "Digite 5 para consultar a media de idade da população"
  putStrLn "Digite 6 imprimir"
  putStrLn "Digite 0 para sair"
  putStr "Opção: "
  opt <- getChar
  getChar -- descarta o Enter
  case opt of
    '1' -> do
      db <- adicionarPessoa dados
      let aux = registroOrdenado db
      putStrLn "Cadastrando pessoa"
      menu aux
    '2' -> do
      putStrLn "Digite o nome: "
      nomeAux <- getLine
      let res = encontrarPessoa dados nomeAux
      print res
      menu dados
    '3' -> do
      putStrLn "Digite o nome: "
      nomeAux <- getLine
      putStrLn "Edite o registro: "
      let pessoa = encontrarPessoa dados nomeAux
      print pessoa
      let res = removerPessoa dados nomeAux
      db <- adicionarPessoa res
      let aux = registroOrdenado db
      menu aux
    '4' -> do
      putStrLn "Digite o nome da cidade: "
      cidade <- getLine
      let populacao = populacaoCidade dados cidade
      putStrLn "População: "
      print populacao
      menu dados
    '5' -> do
      putStrLn "\nMedia de idade da populacao: "
      let media = mediaIdadePopulacao dados
      print media
      menu dados
    '6' -> do
      imprimir dados
      -- putStrLn "\nItem removido com sucesso"
      menu dados
    '0' -> do
      putStrLn "\n--------FIM--------"
    _ -> do
      putStrLn "\nOpção inválida!"
      menu dados

main :: IO ()
main = do
  menu []
  return ()
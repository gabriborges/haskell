
--a

filtroTamanho::Int->[String]->[String]
filtroTamanho _ [] = []
filtroTamanho tamanho (x:xs)
    | length x >= tamanho = x : filtroTamanho tamanho xs
    | otherwise = filtroTamanho tamanho xs

auxfiltroTamanho = filtroTamanho 4 ["casa", "sol", "escola", "oi"]

--b

auxGetPalavra::String->String
auxGetPalavra [] = []
auxGetPalavra (x:xs)
    | [x] /= " "= x:auxGetPalavra xs
    | otherwise = []

getPalavra::String->[String]
getPalavra [] = []
getPalavra lista = palavra : restante_de_palavras
    where
        tamanho = length (auxGetPalavra lista)
        palavra = auxGetPalavra lista
        restante_de_palavras = getPalavra (drop (tamanho+1) lista)

contarPalavras::String->[String]->Int
contarPalavras _ []= 0
contarPalavras palavra lista
    | head lista == palavra = 1 + contarPalavras palavra (tail lista) 
    | otherwise = contarPalavras palavra (tail lista)

auxAnalise::String->[String]->[(String,Int)]
auxAnalise _ [] = []
auxAnalise frase (x:xs) = (x, contarPalavras x (getPalavra frase)) : auxAnalise frase xs


analise::String->[(String,Int)]
analise frase = auxAnalise frase (getPalavra frase)

------------------------------------------
--Sem repetir palavras.

-- auxGetPalavra::String->String
-- auxGetPalavra [] = []
-- auxGetPalavra (x:xs)
--     | [x] /= " "= x:auxGetPalavra xs
--     | otherwise = []

-- getPalavra::String->[String]
-- getPalavra [] = []
-- getPalavra lista = palavra : restante_de_palavras
--     where
--         tamanho = length (auxGetPalavra lista)
--         palavra = auxGetPalavra lista
--         restante_de_palavras = getPalavra (drop (tamanho+1) lista)

-- contarPalavras::String->[String]->Int
-- contarPalavras _ []= 0
-- contarPalavras palavra lista
--     | head lista == palavra = 1 + contarPalavras palavra (tail lista) 
--     | otherwise = contarPalavras palavra (tail lista)

-- removerPalavra::String->[String]->[String]
-- removerPalavra _ [] = []
-- removerPalavra palavra lista
--     | palavra == head lista = removerPalavra palavra (tail lista)
--     | otherwise = head lista : removerPalavra palavra (tail lista)

-- auxAnalise::[String]->[(String,Int)]
-- auxAnalise [] = []
-- auxAnalise lista = (head lista, contarPalavras (head lista) lista) : auxAnalise (removerPalavra (head lista) lista)

-- analise::String->[(String,Int)]
-- analise frase = auxAnalise (getPalavra frase)

------------------------------------------------
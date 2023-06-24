import Data.Char (ord)

--1

pertence::Int->[Int]->Bool
pertence _ [] = False
pertence n (x:xs) 
    | x == n = True
    | otherwise = pertence n xs

--2

maiorElemento::[Int]->Int
maiorElemento [] = 0
maiorElemento [n] = n
maiorElemento (x:y:xs)
    | y>x = maiorElemento (y:xs)
    | otherwise = maiorElemento (x:xs)

--3

n_esimo::Int->[Int]->Int
n_esimo _ [] = 0
n_esimo index lista = lista!!(index-1)

--4

remove_n_esimo::Int->String->String
remove_n_esimo _ [] = "Lista vazia"
remove_n_esimo index lista = take (index-1) lista ++ drop (index) lista

--5

inverte::String->String
inverte [] = []
inverte (a:as) = inverte as ++ [a]

palindromo::String->Bool
palindromo [] = True
palindromo n = n == inverte n

--6

elimina::String->String
elimina [] = ""
elimina [x] = [x]
elimina (x:y:xs)
    | x == y = elimina (x:xs)
    | otherwise = [x] ++ elimina (y:xs)

--7

duplica::String->String
duplica [] = []
duplica (x:xs) = [x]++[x]++duplica xs

--8

numeroAscii :: Char -> Int
numeroAscii = ord

primeira_palavra::String->String
primeira_palavra [] = []
primeira_palavra (x:xs) 
    | (ord x >64 && ord x <91) || (ord x >96 && ord x <123) || (ord x >127 && ord x <166) = [x] ++ primeira_palavra xs
    | otherwise = []

--9

moverDireita::String->Int->String
moverDireita [] _ = []
moverDireita lista n
    | n == 0 = lista
    | otherwise = drop((length(lista)-n)) lista++drop 0 (lista)

--10

intersecao::[Int]->[Int]->[Int]
intersecao lista1 lista2 = [x | x<-lista1, y<-lista2, x==y]
-- intersecao (x:xs) (y:ys) = []

--11

mySplit::Int->[Int]->([Int],[Int])
mySplit n lista = ([x | x<-lista, x<n],[x | x<-lista, x>n])

--12

extrair::Int->Int->[t]->[t]
extrair n m lista
    | n == m = [lista!!(n-1)]
    | otherwise = [lista!!(n-1)] ++ extrair (n+1) m lista

--13

duplicata::String->String
duplicata [] = []
duplicata [n] = [n]
duplicata (x:y:xs)
    | x == y = [x]++duplicata(y:xs)
    | otherwise = [x]

empacoteDuplicata::String->[String]
empacoteDuplicata [] = []
empacoteDuplicata [n] = [[n]]
empacoteDuplicata lista = [duplicata lista] ++ empacoteDuplicata restante
    where
        restante = drop (length(duplicata lista)) lista

--14

reg::[(Int, String)]
reg = [(15, "Ana"), (22, "Pedro"), (2, "Maria"), (12, "João"), (14, "Pablo"), (23, "Poliana")]

encontrarMenor::[(Int, String)]->(Int, String)
encontrarMenor [(idade, nome)] = (idade, nome)
encontrarMenor ((idade, nome):xs) = menor (idade, nome) (encontrarMenor xs)
    where
        menor (idade1, nome1) (idade2, nome2)  = if idade1>=idade2 then (idade1, nome1) else (idade2, nome2)

removerMenor::(Int, String)->[(Int, String)]->[(Int, String)]
removerMenor _ [] = []
removerMenor tupla (x:xs)
    | tupla == x = xs
    | otherwise = [x] ++ removerMenor tupla xs

ordena::[(Int, String)]->[(Int, String)]
ordena [] = []
ordena lista = menor : ordena (removerMenor menor lista)
    where 
        menor = encontrarMenor lista

registroOrdenado = ordena reg

--15

intercala::[t]->[t]->[t]
intercala [] [] = []
intercala xs [] = xs
intercala [] ys = ys
intercala (x:xs) (y:ys) 
    | null [x] && not (null [y])=  y:intercala xs ys
    | null [y] && not (null [x])=  x:intercala xs ys
    | otherwise = x:y:intercala xs ys

--16

contador::Int->[Int]->Int
contador _ [] = 0
contador num (x:xs)
    | x==num = 1 + contador num xs
    | otherwise = 0

contarRepetidos::[Int]->[[Int]]
contarRepetidos [] = []
contarRepetidos [x] = []
contarRepetidos (x:y:xs)
    | x==y = [[quantidade]++[x]] ++ contarRepetidos restante
    | otherwise = [[1]++[x]] ++ contarRepetidos (y:xs)
        where 
            quantidade = contador x (x:y:xs) 
            restante = drop quantidade (x:y:xs)

--17
auxListaSeguida::[Int]->[l]->[l]
auxListaSeguida [] [] = []
auxListaSeguida indexes lista = [lista!!(x-1) | x<-indexes]

listaSeguida::[Int]->[l]->[l]
listaSeguida indexes lista = lista ++ auxListaSeguida indexes lista

--18

metade::[t]->([t],[t])
metade lista = (take meio lista,drop meio lista)
    where
        meio = div (length lista) 2

--19

adicionaNoFinal::[t]->t->[t]
adicionaNoFinal lista x = lista ++ [x]

--20

descontoPassagemDeAviao::[(Float,Int)]->[Float]
descontoPassagemDeAviao [] = []
descontoPassagemDeAviao ((valor, idade):xs)
    | idade<2 = [valor*0.1] ++ descontoPassagemDeAviao xs
    | idade<=10 = [valor*0.5] ++ descontoPassagemDeAviao xs
    | idade >=60 =[valor*0.6] ++ descontoPassagemDeAviao xs
    | otherwise = [valor] ++ descontoPassagemDeAviao xs

--21


--22

divprop::Int->[Int]
divprop n = [x |x<-[1..n-1], mod n x == 0]

--23

ehPerfeito::Int->[Int]->Bool
ehPerfeito n lista
    | n == sum lista = True
    | otherwise = False

perfeitos::Int->[Int]
perfeitos n = [x | x<-[1..n], ehPerfeito x (divprop x)]

--24

pitagoricos::Int->[(Int,Int,Int)]
pitagoricos n = [(x,y,z) | x<-[1..n], y<-[1..n], z<-[1..n], x*x+y*y==z*z]

--25

alfabeto::String->Int
alfabeto "A" = 1
alfabeto "B" = 2
alfabeto "C" = 3
alfabeto "D" = 4
alfabeto "E" = 5
alfabeto "F" = 6
alfabeto "G" = 7
alfabeto "H" = 8
alfabeto "I" = 9
alfabeto "J" = 10
alfabeto "K" = 11
alfabeto "L" = 12
alfabeto "M" = 13
alfabeto "N" = 14
alfabeto "O" = 15
alfabeto "P" = 16
alfabeto "Q" = 17
alfabeto "R" = 18
alfabeto "S" = 19
alfabeto "T" = 20
alfabeto "U" = 21
alfabeto "V" = 22
alfabeto "W" = 23
alfabeto "X" = 24
alfabeto "Y" = 25
alfabeto "Z" = 26
alfabeto _ = -999

alfabetoInv::Int->String
alfabetoInv 1 = "A" 
alfabetoInv 2 = "B" 
alfabetoInv 3 = "C"
alfabetoInv 4 = "D"
alfabetoInv 5 = "E"
alfabetoInv 6 = "F"
alfabetoInv 7 = "G"
alfabetoInv 8 = "H"
alfabetoInv 9 = "I" 
alfabetoInv 10 = "J"
alfabetoInv 11 = "K" 
alfabetoInv 12 = "L"
alfabetoInv 13 = "M"  
alfabetoInv 14 = "N"
alfabetoInv 15 = "O"
alfabetoInv 16 = "P" 
alfabetoInv 17 = "Q"
alfabetoInv 18 = "R"
alfabetoInv 19 = "S"
alfabetoInv 20 = "T"
alfabetoInv 21 = "U" 
alfabetoInv 22 = "V"
alfabetoInv 23 = "W"
alfabetoInv 24 = "X"
alfabetoInv 25 = "Y" 
alfabetoInv 26 = "Z" 
alfabetoInv _ = " "

conv::Int->Int
conv n
    | n>26 = n-26
    | n<=0 = n+26
    | otherwise = n

cifrar::Int->String->String
cifrar _ [] = ""
cifrar n (x:xs) = alfabetoInv(conv (alfabeto [x] + n)) ++ cifrar n (xs)


--26

verificarTamanho::String->Bool
verificarTamanho [] = False
verificarTamanho senha = length senha >= 8

verificarMinuscula::String->Bool
verificarMinuscula [] = False
verificarMinuscula senha = (ord (head senha)>96 && ord (head senha)<123) || verificarMinuscula (tail senha)

verificarMaiscula::String->Bool
verificarMaiscula [] = False
verificarMaiscula senha = (ord (head senha)>64 && ord (head senha)<91) || verificarMaiscula (tail senha)

verificarAlgarismo::String->Bool
verificarAlgarismo [] = False
verificarAlgarismo senha = (ord (head senha)>47 && ord (head senha)<58) || verificarAlgarismo (tail senha)

forte::String->Bool
forte senha = verificarTamanho senha && verificarMinuscula senha && verificarMaiscula senha && verificarAlgarismo senha

--27

-- eliminaRepetidos::(Eq a)=>a->[a]->[a]
-- eliminaRepetidos elem lista = [x | x<-lista, elem/=x]

essencia::(Eq a)=>[a]->[a]
essencia [] = []
essencia [x] = [x]
essencia lista = head lista :  essencia ([x | x<-lista, head lista/=x])

--28

intersperse::a->[a]->[a]
intersperse _ [x] = [x]
intersperse elem lista = head lista : elem : intersperse elem (tail lista)

--29

toBits::Int->[Int]
toBits 1 = [1]
toBits decimal = toBits (div decimal 2) ++ [mod decimal 2]

--30
valorNoIndex::Int->Int
valorNoIndex num = 2^(num-1)

fromBits::[Int]->Int
fromBits [] = 0
fromBits binario 
    | head binario == 1 = 2^((length binario)-1) + fromBits (tail binario)
    | otherwise = fromBits (tail binario)
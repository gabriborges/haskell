import Data.Char (isUpper)

--1

totalFiltro::(t->Bool)->[t]->Int
totalFiltro func lista = length [x | x<-lista, func x]

--2

descompressao::(Eq t)=>[(t,Int)]->[t]
descompressao [] = []
descompressao [(s,n)] = [s | x<-[1..n]]
descompressao ((char,num):xs) = [char | x<-[1..num]] ++ descompressao xs
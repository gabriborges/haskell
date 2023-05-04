fatorial::Integer->Integer
fatorial n
    | n == 0 = 1
    | otherwise = fatorial(n-1) * n

--a)

nCatalan::Integer->Integer
nCatalan n = div (fatorial(n*2)) ((fatorial(n+1))*(fatorial n))

--b)

auxPertenceCatalan::Integer->Integer->Bool
auxPertenceCatalan n i
    | nCatalan i == n = True
    | nCatalan i > n = False
    | otherwise = auxPertenceCatalan n (i+1)

pertenceCatalan::Integer->Bool
pertenceCatalan n = auxPertenceCatalan n 1

--c)

auxQtdCatalan::Integer->Integer->Integer
auxQtdCatalan n i
    | nCatalan i >= n = 0 
    | (pertenceCatalan(nCatalan i)) && (nCatalan i<=n) = 1 + auxQtdCatalan n (i+1)
    | otherwise = 0

qtdCatalan::Integer->Integer
qtdCatalan n = auxQtdCatalan n 0

--d)

auxSomaCatalan::Integer->Integer->Integer->Integer
auxSomaCatalan a1 a2 i
    | nCatalan i > a2 = 0
    | (nCatalan i >= a1) && (nCatalan i <= a2) = (nCatalan i) + (auxSomaCatalan a1 a2 (i+1))
    | otherwise = auxSomaCatalan a1 a2 (i+1)

somaCatalan::Integer->Integer->Integer
somaCatalan n1 n2 = auxSomaCatalan n1 n2 0

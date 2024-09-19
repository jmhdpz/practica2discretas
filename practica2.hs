--------------- Funciones auxiliares ---------------
maximo :: (Num a, Ord a) => a -> a -> a
maximo x y = if x==y
                   then x
                 else if x>y
                      then x
                 else y
--------------- Listas y recursión ---------------

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1+longitud (xs)

sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista (xs)

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento (x:xs) a True = [a] ++ (x:xs)
agregaElemento [] a False = [a]
agregaElemento (x:xs) a False = [x] ++ agregaElemento xs a False

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [x] = x
maximoLista (x:xs) = maximo x (maximoLista xs)

indice :: [a] -> Int -> a
indice [a] 0 = a
indice (x:xs) n = if n > longitud (x:xs)
                     then error "La lista no tiene tantos elementos"
                  else if n < 0
                     then error "Indice negativo"
                  else
                     indice xs (n-1)

--------------- Listas por comprehensión ---------------

divisores :: Int -> [Int]
divisores _ = undefined

conjunto :: Eq a => [a] -> [a]
conjunto _ = undefined

numerosPares :: [Int] -> [Int]
numerosPares _ = undefined

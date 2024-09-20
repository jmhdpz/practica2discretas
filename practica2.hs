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
indice (x:xs) 0 = x
indice (x:xs) n = if n >= longitud (x:xs) || n<0
                     then error "El indice dado no pertenece al rango"
                  else if n == 0 
                     then x
                  else 
                     indice (xs) (n-1)

--------------- Listas por comprehensión ---------------

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], mod n x == 0]

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto[y | y <- xs , y /= x ]

numerosPares :: [Int] -> [Int]
numerosPares (x:xs) = [n | n <- (x:xs), mod n 2 == 0]

-- 1.1
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--1.2
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--1.3
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = x+1 : sucesores xs

--1.4
conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (x:xs) = x && conjuncion xs

--1.5
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs

--1.6
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs:xss) =  xs ++ aplanar xss

--1.7
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

--1.8
apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x:xs) = if e == x
	then 1 + apariciones e xs
	else apariciones e xs

--1.9
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ [] = []
losMenoresA n (x:xs) = if x < n
	then x : losMenoresA n xs
	else losMenoresA n xs

--1.10
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (xs:xss) = if longitud xs > n
	then xs : lasDeLongitudMayorA n xss
	else lasDeLongitudMayorA n xss

--1.11
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] a = [a]
--agregarAlFinal x a = x ++ [a]
agregarAlFinal (x:xs) a = x : agregarAlFinal xs a

--1.12
concatenar :: [a] -> [a] -> [a]
concatenar [] ys = ys
concatenar (x:xs) ys = x : concatenar xs ys

--1.13
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) =  reversa xs ++ [x]

--1.14
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] ys = ys
zipMaximos xs [] = xs
zipMaximos (x:xs) (y:ys) = mayor x y : zipMaximos xs ys

mayor :: Int -> Int -> Int
mayor a b = if a > b then a else b

--1.15
elMinimo :: Ord a => [a] -> a
elMinimo [] = error "No hay minimo de vacio"
elMinimo [x] = x
elMinimo (x:xs) = menor x (elMinimo xs)

menor :: Ord a => a -> a -> a
menor a b = if a < b then a else b

--2.1
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--2.2
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 1 = [1]
cuentaRegresiva  n = if n >= 1
	then n : cuentaRegresiva (n-1)
	else error "No se puede hacer cuenta regresiva"

--2.3
repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n a = a : repetir (n-1) a

--2.4
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros _ [] = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

--2.5
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs = xs
sinLosPrimeros _ [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs



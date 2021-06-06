--1
--1.1
data Color = Azul | Rojo deriving (Eq, Show)
data Celda = Bolita Color Celda | 
             CeldaVacia deriving Show

celda1 = Bolita Azul (Bolita Azul (Bolita Rojo CeldaVacia))

nroBolitas :: Color -> Celda -> Int
nroBolitas col CeldaVacia = 0
nroBolitas col (Bolita c cel)  = (if col == c then 1 else 0) + nroBolitas col cel

poner :: Color -> Celda -> Celda
poner col CeldaVacia = (Bolita col CeldaVacia)
poner col (Bolita c cel) = (Bolita c (poner col cel))

sacar :: Color -> Celda -> Celda
sacar col CeldaVacia = CeldaVacia
sacar col (Bolita c cel) = if col == c 
	then cel
	else (Bolita c (sacar col cel))


ponerN :: Int -> Color -> Celda -> Celda
ponerN 1 col cel = poner col cel
ponerN n col cel = ponerN (n-1) col (poner col cel)

--1.2
data Objeto = Cacharro | 
              Tesoro deriving (Eq, Show)
data Camino = Fin | 
              Cofre [Objeto] Camino | 
              Nada           Camino deriving Show

camino1 = Fin
camino2 = Nada Fin
camino3 = Cofre [Tesoro] camino2
camino4 = Cofre [Tesoro, Cacharro] camino2
camino5 = Nada  (Nada (Cofre [Tesoro, Tesoro] Fin))

hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada camino) = hayTesoro camino
hayTesoro (Cofre obj camino) = hayTesoroEnObj obj || hayTesoro camino

hayTesoroEnObj:: [Objeto] -> Bool
hayTesoroEnObj [] = False
hayTesoroEnObj (x:xs) = x == Tesoro || hayTesoroEnObj xs

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro (Nada camino) = 1 + pasosHastaTesoro camino
pasosHastaTesoro (Cofre l camino) = if not (hayTesoroEnObj l)
	then 1 + pasosHastaTesoro camino
	else 0


hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn _ Fin = False
hayTesoroEn 0 (Nada camino) = False
hayTesoroEn 0 (Cofre l camino) = hayTesoroEnObj l 
hayTesoroEn n (Nada camino) = hayTesoroEn (n-1) camino
hayTesoroEn n (Cofre l camino) = hayTesoroEn (n-1) camino

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n camino = n < cantidadTesoros camino

cantidadTesoros:: Camino -> Int
cantidadTesoros Fin = 0
cantidadTesoros (Nada camino) = cantidadTesoros camino
cantidadTesoros (Cofre l camino) = cantTesoros l + cantidadTesoros camino

cantTesoros:: [Objeto] -> Int
cantTesoros [] = 0
cantTesoros (x:xs) = (if x==Tesoro then 1 else 0) + cantTesoros xs

cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre n m camino = if n /= m 
	then cantTesorosEn m camino - cantTesorosEn n camino
	else cantTesorosEn m camino

cantTesorosEn:: Int -> Camino -> Int
cantTesorosEn _ Fin = 0
cantTesorosEn 0 (Nada camino) = 0
cantTesorosEn 0 (Cofre l camino) = cantTesoros l 
cantTesorosEn n (Nada camino) = cantTesorosEn (n-1) camino
cantTesorosEn n (Cofre l camino) = cantTesoros l + cantTesorosEn (n-1) camino


--2
--2.1
data Tree a = EmptyT | 
              NodeT a (Tree a) (Tree a) deriving Show

arbol1:: Tree Int
arbol1 = NodeT 10 
           (NodeT 20 
           	 (NodeT 40 EmptyT EmptyT)
           	 (NodeT 50 EmptyT EmptyT))
           (NodeT 30 
           	 (NodeT 60 EmptyT EmptyT)
           	 (NodeT 70 EmptyT EmptyT))
         
--2.1.1
sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT  n ti td) = n + sumarT ti + sumarT td

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT n ti td) = 1 + sizeT ti + sizeT td

--1.1.a
sucesor :: Int -> Int
sucesor x = x + 1

--1.1.b
sumar :: Int -> Int -> Int
sumar x y = x + y

--1.1.c
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x 0 = error "No pongas un cero"
divisionYResto x y = (div x y, mod x y)

--1.1.d
maxDelPar :: (Int,Int) -> Int
maxDelPar (x, y) = if x > y then x else y

--1.2
diez1 = sucesor (maxDelPar (8,9))
diez2 = maxDelPar (sumar 4 6, sucesor 3)
diez3 = sumar (sucesor 3) (maxDelPar (2,6))
diez4 = sucesor (sucesor (maxDelPar (8,1)))

--2.1.a
data Dir = Norte | Sur | Este | Oeste deriving Show

opuesto :: Dir -> Dir
opuesto Sur = Norte
opuesto Norte = Sur
opuesto Este = Oeste
opuesto Oeste = Este

--2.1.b
iguales :: Dir -> Dir -> Bool
iguales Sur Sur = True
iguales Norte Norte = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False

--2.1.c
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste =  error "No hay siguiente al Oeste"

--2.2.a
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

--2.2.b
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

--2.2.c
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Lunes Domingo = True
vieneDespues Martes Lunes = True
vieneDespues Miercoles Martes = True
vieneDespues Jueves Miercoles = True
vieneDespues Viernes Jueves = True
vieneDespues Sabado Viernes = True
vieneDespues Domingo Sabado = True
vieneDespues _ _ = False

--2.2.d
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

--2.3.a
negar :: Bool -> Bool
negar True = False
negar False = True

--2.3.b
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

--2.3.c
and :: Bool -> Bool -> Bool
and True True = True
and _ _ = False

--2.3.d
or :: Bool -> Bool -> Bool
or False False = False
or _ _  = True

--3.1
data Persona =  Constru String Int deriving Show

nombre :: Persona -> String
nombre (Constru nom eda) = nom


edad :: Persona -> Int
edad (Constru nom eda) = eda

crecer :: Persona -> Persona
crecer (Constru nom eda) = (Constru nom (eda+1))

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre n (Constru nom eda) = (Constru n eda)

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (Constru nom1 eda1) (Constru nom2 eda2) = eda1 > eda2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor x y = if esMayorQueLaOtra x y then x else y

--3.2
data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon =  ConstruPK TipoDePokemon Int deriving Show
data Entrenador = ConstruE String Pokemon Pokemon deriving Show

poke1 :: Pokemon
poke1 = ConstruPK Agua 10
poke2 :: Pokemon
poke2 = ConstruPK Fuego 20
poke3 :: Pokemon
poke3 = ConstruPK Fuego 50
poke4 :: Pokemon
poke4 = ConstruPK Planta 5

pepe :: Entrenador
pepe = ConstruE "Pepe" poke2 poke3
pipo :: Entrenador
pipo = ConstruE "Pipe" poke1 poke4

superaA :: Pokemon -> Pokemon -> Bool
superaA (ConstruPK Agua p) (ConstruPK Fuego p2) = True
superaA (ConstruPK Fuego p) (ConstruPK Planta p2) = True
superaA (ConstruPK Planta p) (ConstruPK Agua p2) = True
superaA pok1 pok2 = False

cantidadDePokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonesDe t (ConstruE n (ConstruPK t1 x) (ConstruPK t2 x1)) = unoSiIgual t t1 + unoSiIgual t t2

unoSiIgual :: TipoDePokemon -> TipoDePokemon ->Int
unoSiIgual Agua Agua = 1
unoSiIgual Fuego Fuego = 1
unoSiIgual Planta Planta = 1
unoSiIgual _ _ =0

juntarPokemones :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemones ( (ConstruE n pk1 pk2), (ConstruE n2 pk3 pk4) ) = [pk1,pk2,pk3,pk4]

--4.1.a
loMismo :: a -> a
loMismo a = a

--4.1.b
siempreSiete :: a -> Int
siempreSiete _ = 7

--4.1.c
swap :: (a,b) -> (b, a)
swap (a,b) = (b,a)

--5.2
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

--5.3
elPrimero :: [a] -> a
elPrimero (x:xs) = x
elPrimero [] = error "lista vacia"

--5.4
sinElPrimero :: [a] -> [a]
sinElPrimero (x:xs) = xs
sinElPrimero [] = []

--5.5
splitHead :: [a] -> (a, [a])
splitHead (x:xs) = (x, (x:xs))
splitHead [] = error "lista vacia"


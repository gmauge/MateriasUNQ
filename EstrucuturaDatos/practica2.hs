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

--3.1a
data Persona = Per String Int deriving Show

perona1 = Per "Jose" 31
perona2 = Per "Pepe" 20
perona3 = Per "Ale" 18
perona4 = Per "Raul" 55

personas = [perona1,perona2,perona3,perona4]

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n []  = []
mayoresA 0 xs = xs
mayoresA n (x:xs) = personaMayor n x ++ mayoresA n xs

personaMayor:: Int -> Persona -> [Persona]
personaMayor n (Per nom ed) = if ed > n
	then [(Per nom ed)]
	else []

--3.1b
promedioEdad :: [Persona] -> Int
promedioEdad p =  div (sumaEdades p) (length p)

sumaEdades:: [Persona] -> Int
sumaEdades [] = 0
sumaEdades (x:xs) = edad x + sumaEdades xs

edad:: Persona -> Int
edad (Per _ n) = n

--3.1c
elMasViejo :: [Persona] -> Persona
elMasViejo [p] = p
elMasViejo (x:xs) = mayorEntre x (elMasViejo xs)

mayorEntre:: Persona -> Persona -> Persona
mayorEntre p1 p2 = if edad p1 > edad p2
	then p1
	else p2

--3.2
data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = ConsPokemon TipoDePokemon Int deriving Show
data Entrenador = ConsEntrenador String [Pokemon] deriving Show

poke1 = ConsPokemon Agua 10
poke2 = ConsPokemon Fuego 1
poke3 = ConsPokemon Fuego 100
poke4 = ConsPokemon Planta 50
poke5 = ConsPokemon Planta 5
poke6 = ConsPokemon Planta 500
entre1 = ConsEntrenador "Jose" [poke1, poke2, poke3, poke4]
entre2 = ConsEntrenador "Luis" [poke5, poke6]
entre3 = ConsEntrenador "Luis" []

--3.2a
cantPokemones :: Entrenador -> Int
cantPokemones (ConsEntrenador _ lPok) = length lPok

--3.2b
cantPokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonesDe tip (ConsEntrenador _ lPok) = cantidadDeTipo tip lPok

cantidadDeTipo:: TipoDePokemon -> [Pokemon] -> Int
cantidadDeTipo _ [] = 0
cantidadDeTipo tip (x:xs) = unoSiMismoTipo tip x + cantidadDeTipo tip xs

unoSiMismoTipo:: TipoDePokemon -> Pokemon -> Int
unoSiMismoTipo Agua (ConsPokemon Agua _) = 1
unoSiMismoTipo Fuego (ConsPokemon Fuego _) = 1
unoSiMismoTipo Planta (ConsPokemon Planta _) = 1
unoSiMismoTipo _ _ = 0

--3.2c
losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan tip (ConsEntrenador _ ls1) (ConsEntrenador _ ls2) = cuantosLeGanan tip ls1 ls2

cuantosLeGanan:: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
cuantosLeGanan tip l1 l2 =  cuentaLosQueGanan (losDeTipo tip l1) l2

losDeTipo:: TipoDePokemon -> [Pokemon] -> [Pokemon]
losDeTipo _ [] = []
losDeTipo tip (x:xs) = pokSiEsDeTipo tip x ++ losDeTipo tip xs

pokSiEsDeTipo::TipoDePokemon -> Pokemon -> [Pokemon]
pokSiEsDeTipo Agua (ConsPokemon Agua i) = [(ConsPokemon Agua i)]
pokSiEsDeTipo Fuego (ConsPokemon Fuego i) = [(ConsPokemon Fuego i)]
pokSiEsDeTipo Planta (ConsPokemon Planta i) = [(ConsPokemon Planta i)]
pokSiEsDeTipo _ _ = []

cuentaLosQueGanan:: [Pokemon] -> [Pokemon] -> Int
cuentaLosQueGanan [] _ = 0
cuentaLosQueGanan (x:xs) l2 = unoSiGanaATodos x l2 + cuentaLosQueGanan xs l2

unoSiGanaATodos:: Pokemon -> [Pokemon] -> Int
unoSiGanaATodos (ConsPokemon t _) l2 = if ganaTodos t l2 then 1 else 0

ganaTodos:: TipoDePokemon -> [Pokemon] -> Bool
ganaTodos _ [] = True
ganaTodos t (x:xs) = gana t x && ganaTodos t xs

gana::TipoDePokemon -> Pokemon -> Bool
gana Agua (ConsPokemon Fuego _) = True
gana Fuego (ConsPokemon Planta _) = True
gana Planta (ConsPokemon Agua _) = True
gana _ _ = False

--3.2d
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ ls) =   posee Agua ls 
										&& posee Fuego ls
										&& posee Planta ls

posee:: TipoDePokemon -> [Pokemon] -> Bool
posee _ [] = False
posee t (x:xs) = esDeTipo t x || posee t xs

esDeTipo:: TipoDePokemon -> Pokemon -> Bool
esDeTipo Agua (ConsPokemon Agua i) = True
esDeTipo Fuego (ConsPokemon Fuego _) = True
esDeTipo Planta (ConsPokemon Planta _) = True
esDeTipo _ _ = False


--3.3
data Seniority = Junior | SemiSenior | Senior deriving Show
data Proyecto = ConsProyecto String deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa = ConsEmpresa [Rol] deriving Show

proy1 = ConsProyecto "Poyecto 1" 
proy2 = ConsProyecto "Poyecto 2" 
proy3 = ConsProyecto "Poyecto 3" 

rol1 = Developer Junior proy1
rol2 = Management Junior proy1
rol3 = Developer SemiSenior proy2
rol4 = Management SemiSenior proy2
rol5 = Developer Senior proy3
rol6 = Management Senior proy3

emp = ConsEmpresa [rol1, rol2, rol3, rol4, rol5, rol6]

--3.3.a
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa []) = []
proyectos (ConsEmpresa rols) = sinRepetidos (listaProyectos rols)

listaProyectos:: [Rol] -> [Proyecto]
listaProyectos [] = []
listaProyectos (x:xs) = proyecto x ++ listaProyectos xs

proyecto:: Rol -> [Proyecto]
proyecto (Developer _ p) = [p]
proyecto (Management _ p) = [p]

sinRepetidos:: [Proyecto] -> [Proyecto]
sinRepetidos [] = []
sinRepetidos (x:xs) = sacaSiEsta x xs ++ sinRepetidos xs

sacaSiEsta:: Proyecto -> [Proyecto] -> [Proyecto]
sacaSiEsta p [] = [p]
sacaSiEsta p (x:xs) = if esElMismo p x 
	then []
	else sacaSiEsta p xs

esElMismo:: Proyecto -> Proyecto -> Bool
esElMismo (ConsProyecto s1) (ConsProyecto s2) = s1 == s2

--3.3b
losDevSenior :: Empresa -> Int
losDevSenior (ConsEmpresa ls) = losSenior ls

losSenior:: [Rol] -> Int
losSenior [] = 0
losSenior (x:xs) = unoSiSinior x + losSenior xs

unoSiSinior:: Rol -> Int
unoSiSinior (Developer Senior _) = 1
unoSiSinior _ = 0

--3.3c
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn [] _ = 0
cantQueTrabajanEn (x:xs) emp = trabajanEn x emp + (cantQueTrabajanEn xs emp)

trabajanEn:: Proyecto -> Empresa -> Int
trabajanEn p (ConsEmpresa ls) = trabajanEn' p ls

trabajanEn'::Proyecto -> [Rol] -> Int
trabajanEn' _ [] = 0
trabajanEn' p (x:xs) = (unoSiMismoProyecto p x) + trabajanEn' p xs

unoSiMismoProyecto:: Proyecto -> Rol -> Int
unoSiMismoProyecto p1 (Developer _ p2) = unoSiMismo p1 p2
unoSiMismoProyecto p1 (Management _ p2) = unoSiMismo p1 p2

unoSiMismo:: Proyecto -> Proyecto -> Int
unoSiMismo (ConsProyecto s1) (ConsProyecto s2) = if s1 == s2 then 1 else 0

--3.3.d
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto emp = asignados (proyectos emp) emp

asignados:: [Proyecto] -> Empresa -> [(Proyecto, Int)]
asignados [] _ = []
asignados (x:xs) emp = (x, (cantQueTrabajanEn [x] emp)) : asignados xs emp

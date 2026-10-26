module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--Guía de Ejercicios 1:

--Ejercicio 1
esMultiploDeTres :: Number -> Bool
esMultiploDeTres x = mod x 3 == 0

--Ejercicio 2
esMultiploDe :: Number -> Number -> Bool
esMultiploDe x y = mod x y == 0

--Ejercicio 3
cubo :: Number -> Number
cubo x = x^3

--Ejercicio 4
area :: Number -> Number -> Number
area x y = x * y / 2

--Ejercicio 5
esBisiesto :: Number -> Bool
esBisiesto anio = esMultiploDe anio 400 || (esMultiploDe anio 4 && esMultiploDe anio 100)

--Ejercicio 5 (otra alternativa)
esBisiesto2 :: Number -> Bool
esBisiesto2 anio
    | esMultiploDe anio 400 = True
    | esMultiploDe anio 100 = False
    | esMultiploDe anio 4 = True
    | otherwise = False

--Ejercicio 6
celsiusToFahr :: Number -> Number
celsiusToFahr celsius = celsius * 1.8 + 32

--Ejercicio 7
fahrToCelsius :: Number -> Number
fahrToCelsius fahr = (fahr - 32) / 1.8

--Ejercicio 8
haceFrioF :: Number -> Bool
haceFrioF fahr = fahrToCelsius fahr < 8

--Ejercicio 9
mcm :: Number -> Number -> Number
mcm x y = (x * y) / (gcd x y)

--Ejercicio 10.a
dispersion :: Number -> Number -> Number -> Number
dispersion x y z = max x (max y z) - min x (min y z)

--Ejercicio 10.b
diasParejos :: Number -> Number -> Number -> Bool
diasParejos x y z = (dispersion x y z) < 30

diasLocos :: Number -> Number -> Number -> Bool
diasLocos x y z = (dispersion x y z) > 100

diasNormales :: Number -> Number -> Number -> Bool
diasNormales x y z = ((diasParejos x y z) == False) && ((diasLocos x y z) == False)

--Ejercicio 11.a
pesoPino :: Number -> Number
pesoPino altura
    | altura <= 300 = altura * 3
    | altura > 300 = 300 * 3 + (altura - 300) * 2

--Ejercicio 11.b
esPesoUtil :: Number -> Bool
esPesoUtil peso = (peso > 400) && (peso < 1000)

--Ejercicio 11.c
sirvePino :: Number -> Bool
sirvePino = esPesoUtil.pesoPino

--Ejercicio 12
--esCuadradoPerfecto :: Number -> Bool
--esCuadradoPerfecto 

--Guía de Ejercicios 2:

--Ejercicio 1
siguiente :: Number -> Number
siguiente = ( +1)

--Ejercicio 2
mitad :: Number -> Number
mitad = ( /2)

--Ejercicio 3
inversa :: Number -> Number
inversa = (1/ )

--Ejercicio 4
triple :: Number -> Number
triple = (3* )

--Ejercicio 5
esNumeroPositivo :: Number -> Bool
esNumeroPositivo = ( >0)

--Ejercicio 6
esMultiploDeAP :: Number -> Number -> Bool
esMultiploDeAP x = ( == 0).(`mod` x )

--Ejercicio 7
esBisiestoAP :: Number -> Bool
esBisiestoAP anio = (((esMultiploDe anio 400) || ).(esMultiploDe anio 4 && ).not.(esMultiploDe anio )) 100

--Ejercicio 8
inversaRaizCuadrada :: Number -> Number
inversaRaizCuadrada = inversa.sqrt

--Ejercicio 9
incrementMCuadradoN :: Number -> Number -> Number
incrementMCuadradoN = ( + ).( ^2)

--Ejercicio 10
esResultadoPar :: Number -> Number -> Bool
esResultadoPar x = even.(x^ )

--Guía de Ejercicios 3

--Ejercicio 1
fst3 :: (x, y, z) -> x
fst3 (x, _, _) = x

snd3 :: (x, y, z) -> y
snd3 (_, y, _) = y

trd3 :: (x, y, z) -> z
trd3 (_, _, z) = z

--Ejercicio 2
aplicar :: (Number->x, Number->y) -> Number -> (x, y)
aplicar (funcion1, funcion2) z = (funcion1 z, funcion2 z)

--Ejercicio 3
cuentaBizarra :: (Number, Number) -> Number
cuentaBizarra (x, y)
    | x > y = x+y
    | y-x > 10 = y-x
    | (y-x < 10  &&  y-x > 0) = y*x

--Ejercicio 4.a
esNotaBochazo :: Number -> Bool
esNotaBochazo = ( < 6)

--Ejercicio 4.b
aproboG3 :: (Number, Number) -> Bool
aproboG3 (x, y) = ((not.esNotaBochazo) x && (not.esNotaBochazo) y)

--Ejercicio 4.c
promociono :: (Number, Number) -> Bool
promociono (x, y) = (x+y >= 15  &&  x >= 7  &&  y >= 7)

--Ejercicio 4.d
aproboPP :: (Number, Number) -> Bool
aproboPP (x, _) = (not.esNotaBochazo) x

--Ejercicio 5.a
notasFinales :: ((Number, Number), (Number, Number)) -> (Number, Number)
notasFinales ((x, y), (z, t)) = (max x z, max y t)

--Ejercicio 5.b
recursa :: ((Number, Number), (Number, Number)) -> Bool
recursa = not.aproboG3.notasFinales

--Ejercicio 5.c
recuperoPP :: ((Number, Number), (Number, Number)) -> Bool
recuperoPP ((_, _), (z, _)) = not (z == -1)

--Ejercicio 5.d
--Auxiliar
recuperoConProm :: (Number, Number) -> Bool -> Bool
recuperoConProm (c, d) prom = prom && not((c == -1) && (d == -1))

recuperoDeGusto :: ((Number, Number), (Number, Number)) -> Bool
recuperoDeGusto ((a, b), (c, d))= recuperoConProm (c,d) (promociono (a,b))

--Ejercicio 6
--esMayorDeEdad :: (String, Number) -> Bool
--esMayorDeEdad = 

--Ejercicio 7
--calcular :: (Number, Number) -> (Number, Number)
--calcular (x, y) = 

--Guía de Ejercicios 4 (Listas)

--Ejercicio 1
sumaDeListas :: [Number] -> Number
sumaDeListas x = sum x

--Ejercicio 2.a
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]

promedioFrecuenciaCardiaca :: [Number] -> Number
promedioFrecuenciaCardiaca frecuenciaCardiaca = (sumaDeListas frecuenciaCardiaca) / (length frecuenciaCardiaca)

--Ejercicio 2.b
frecuenciaCardiacaMinuto :: Number -> Number
frecuenciaCardiacaMinuto m = frecuenciaCardiaca !! (m/10)

--Ejercicio 2.c
frecuenciasHastaMomento :: Number -> [Number]
frecuenciasHastaMomento m = take (m/10 + 1) frecuenciaCardiaca

--Ejercicio 3
esCapicua :: [String] -> Bool
esCapicua lista = reverse (concat lista) == concat lista

--Ejercicio 4.a
duracionLlamadas :: ((String, [Number]),(String,[Number]))
duracionLlamadas = (("horarioReducido",[20,10,25,15]),("horarioNormal",[10,5,8,2,9,10]))

--cuandoHabloMasMinutos :: duracionLlamadas -> String
--cuandoHabloMasMinutos duracionLlamadas
--    | ((sumaDeListas.snd.fst) duracionLlamadas) > ((sumaDeListas.snd.snd) duracionLlamadas) = (fst.fst) duracionLlamadas
--    | otherwise = (fst.snd) duracionLlamadas

--Ejercicio 4.b
--cuandoHizoMasLlamadas :: duracionLlamadas -> String
--cuandoHizoMasLlamadas duracionLlamadas
--    | ((length.snd.fst) duracionLlamadas) > ((length.snd.snd) duracionLlamadas) = (fst.fst) duracionLlamadas
--    | otherwise = (fst.snd) duracionLlamadas

--Guía de Ejercicios 4 (Orden Superior)

--Ejercicio 1
existsAny :: (Number -> Bool) -> (Number, Number, Number) -> Bool
existsAny f x = ((f.fst3) x || (f.snd3) x || (f.trd3) x)

--Ejercicio 2
mejor :: (Number -> Number) -> (Number -> Number) -> Number -> Number
mejor f g x
    | (f x) > (g x) = (f x)
    | otherwise = (g x)

--Ejercicio 3
aplicarPar :: (x -> y) -> (x, x) -> (y, y)
aplicarPar f xs = ((f.fst) xs, (f.snd) xs)

--Ejercicio 4
parDeFns :: (x -> y) -> (x -> z) -> x -> (y, z)
parDeFns f g x = (f x, g x)

--Guía de Ejercicios 4 (Orden Superior + Listas)

--Ejercicio 1
esMultiploDeAlguno :: Number -> [Number] -> Bool
esMultiploDeAlguno x = any (esMultiploDe x) 

--Ejercicio 2
prom :: [Number] -> Number
prom x = sumaDeListas x / length x

promedios :: [[Number]] -> [Number]
promedios = map prom

--Ejercicio 3
promediosSinAplazos :: [[Number]] -> [Number]
promediosSinAplazos lista = map prom (map (filter (>= 4)) lista)

--Ejercicio 4
mejoresNotas :: [[Number]] -> [Number]
mejoresNotas = map maximum

--Ejercicio 5
aproboG4 :: [Number] -> Bool
aproboG4 = (>=6).minimum

--Ejercicio 6
aprobaron :: [[Number]] -> [[Number]]
aprobaron = filter aproboG4

--Ejercicio 7
divisores :: Number -> [Number]
divisores k = filter (esMultiploDe k) [1 ... k]

--Ejercicio 8
exists :: (a -> Bool) -> [a] -> Bool
exists f = any f

--Ejercicio 9
--hayAlgunNegativo :: [Number] 

--Ejercicio 10
aplicarFunciones :: [Number -> a] -> Number -> [a]
aplicarFunciones [] _ = []
aplicarFunciones (f:fs) x = (f x : aplicarFunciones fs x)

--Ejercicio 11
sumaF :: [Number -> Number] -> Number -> Number
sumaF fs = sumaDeListas.(aplicarFunciones fs)

--Ejercicio 12
subirHabilidad :: Number -> [Number] -> [Number]
subirHabilidad _ [] = []
subirHabilidad x (f:fs)
    | ((f + x) <= 12) = (f + x : subirHabilidad x fs)
    | otherwise = 12 : subirHabilidad x fs

--Ejercicio 13
flimitada :: (Number -> Number) -> Number -> Number
flimitada f = (max 0 ).(min 12 ).f

--Ejercicio 13.a
cambiarHabilidad :: (Number -> Number) -> [Number] -> [Number]
cambiarHabilidad f = map (flimitada f) 

--Ejercicio 13.b
--La respuesta es: cambiarHabilidad (max 4 ) lista

--Ejercicio 14
--Toma elementos de una lista a partir de una función que verifica una condición. Si esta condición deja de cumplirse, deja de tomar elementos y devuelve finalmente la lista formada.

--Ejercicio 15
primerosPares :: [Number] -> [Number]
primerosPares = takeWhile even

primerosDivisores :: Number -> [Number] -> [Number]
primerosDivisores n = takeWhile (esMultiploDe n)

primerosNoDivisores :: Number -> [Number] -> [Number]
primerosNoDivisores n = takeWhile (not.esMultiploDe n)

--Ejercicio 16


--Ejercicio 17.a


--Ejercicio 17.b


--Ejercicio 17.c


--Ejercicio 17.d


--Ejercicio 18.a


--Ejercicio 18.b


--Ejercicio 19
sumatoriaLista :: [Number] -> Number
sumatoriaLista xs = foldl (+) 0 xs

--Ejercicio 20
productoriaLista :: [Number] -> Number
productoriaLista xs = foldl (*) 1 xs

--Ejercicio 21
--dispersion :: [Number] -> Number
--dispersion xs = foldr () 0 xs

--TP de Práctica - Casino Royale

--Estructuras Necesarias
palo = ["Corazones", "Picas", "Tréboles", "Diamantes"]

type Carta = (Number, String)

data Jugador = Jugador{
    nombre :: String,
    mano :: [Carta],
    bebida :: String
}

mesaQueMasAplauda = [jamesBond, leChiffre, felixLeiter]

--Funciones Constantes
pokerDeAses    = [(1,"Corazones"), (1,"Picas"), (1,"Tréboles"), (1,"Diamantes"), (10,"Diamantes")]
fullDeJokers   = [(11,"Corazones"), (11,"Picas"), (11,"Tréboles"), (10,"Diamantes"), (10,"Picas")]
piernaDeNueves = [(9,"Corazones"), (9,"Picas"), (9,"Tréboles"), (10,"Diamantes"), (4,"Copas")]

--Jugadores
jamesBond = Jugador {nombre = "jamesBond", mano = pokerDeAses, bebida = "Martini... shaken, not stirred"}
leChiffre = Jugador {nombre = "leChiffre", mano = fullDeJokers, bebida = "Gin"}
felixLeiter = Jugador {nombre = "felixLeiter", mano = piernaDeNueves, bebida = "Whisky"}

--Funciones Disponibles
ocurrenciasDe x = length . filter (== x)

concatenar = foldl (++) []

--Ejercicio 1.a
mayorSegun' :: (Number -> Number) -> Number -> Number -> Number
mayorSegun' f x1 x2
    | (f x1 > f x2) = f x1
    | (f x1 < f x2) = f x2
    | otherwise = x1

--Ejercicio 1.b
maximoSegun' :: (Number -> Number) -> [Number] -> Number
maximoSegun' f xs = foldl1 (max) (map f xs)

--Ejercicio 1.b (Alternativa con Composición y AP)
maximoSegunCompYAP :: (Number -> Number) -> [Number] -> Number
maximoSegunCompYAP f = (foldl1 max).(map f)

--Ejercicio 1.c
sinRepetidos :: [Number] -> [Number]
sinRepetidos [] = []
sinRepetidos (x:xs) = (x : sinRepetidos (filter (/= x) xs))

--Ejercicio 2.a
esoNoSeVale :: Carta -> Bool
esoNoSeVale carta = not (((fst carta >= 1) && (fst carta <= 13)) && (any (== (snd carta)) palo))

--Ejercicio 2.b
--manoNegra :: Jugador -> Bool
--manoNegra jugador = 

--Ejercicio 3.a


--Ejercicio 3.b


--Ejercicio 3.c


--Ejercicio 3.d


--Ejercicio 3.e


--Ejercicio 3.f


--Ejercicio 4


--Ejercicio 5.a


--Ejercicio 5.b


--Ejercicio 6.a


--Ejercicio 6.b


--Ejercicio 6.c


--Ejercicio 6.d


--Ejercicio 7.a


--Ejercicio 7.b



--Pizza Conmigo

--1.a
data Pizza = Pizza {
    ingredientes :: [String],
    tamanio :: Number,
    calorias :: Number
} deriving Show

--1.b
grandeDeMuzza :: Pizza
grandeDeMuzza = Pizza {
    ingredientes = ["salsa", "mozzarella", "orégano"],
    tamanio = 8,
    calorias = 350
}

--2
nivelDeSatisfaccion :: Pizza -> Number
nivelDeSatisfaccion (Pizza ings _ cal)
    | elem "palmito" ings = 0
    | cal < 500 = valorCalorias
    | otherwise = valorCalorias / 2
    where valorCalorias = (80*).length $ ings 

--3
valorDePizza :: Pizza -> Number
valorDePizza (Pizza ings tam _) = (120 *).(length ings *) $ tam

--4.a
nuevoIngrediente :: String -> Pizza -> Pizza
nuevoIngrediente ingrediente pizza
    | elem ingrediente $ ingredientes pizza = pizza
    | otherwise = pizza { 
                    ingredientes = ingrediente : ingredientes pizza,
                    calorias = (2 +).(calorias pizza *).length $ ingrediente
                }

--4.b
agrandar :: Pizza -> Pizza
agrandar pizza = pizza {tamanio = (min 10 ).(2 +).tamanio $ pizza}

--4.c
mezcladita :: Pizza -> Pizza -> Pizza
mezcladita primeraPizza segundaPizza = segundaPizza {
        ingredientes = mezclaDeIngredientes primeraPizza segundaPizza,
        calorias = calculoCalorias primeraPizza segundaPizza
    }

mezclaDeIngredientes :: Pizza -> Pizza -> [String]
mezclaDeIngredientes primeraPizza segundaPizza = agregarSinRepetir (ingredientes primeraPizza) (ingredientes segundaPizza)

calculoCalorias :: Pizza -> Pizza -> Number
calculoCalorias primeraPizza segundaPizza = (calorias primeraPizza /).(2 +).calorias $ segundaPizza 

agregarSinRepetir base agregados = foldl agregar base agregados
    where agregar base agregado
            | notElem agregado base = agregado : base
            | otherwise = base

--5
nivelDeSatisfaccionPedido :: Pedido -> Number
nivelDeSatisfaccionPedido = sum . map nivelDeSatisfaccion

--6
type Pedido = [Pizza]
type Pizzeria = Pedido -> Pedido

--6.a
pizzeriaLosHijosDePato :: Pizzeria
pizzeriaLosHijosDePato = map (nuevoIngrediente "palmito")

--6.b
pizzeriaElResumen :: Pizzeria
pizzeriaElResumen pedido 
    | length pedido == 1 = pedido
    | otherwise = zipWith mezcladita pedido $ tail pedido

pizzeriaElResumen' :: Pizzeria
pizzeriaElResumen' [pizza] = [pizza]
pizzeriaElResumen' pedido = zipWith mezcladita pedido $ tail pedido

--6.c
pizzeriaEspecial :: Pizza -> Pizzeria
pizzeriaEspecial predilecta = map (mezcladita predilecta)

pizzeriaPescadito :: Pizzeria
pizzeriaPescadito = pizzeriaEspecial pizzaDeAnchoasBasica

pizzaDeAnchoasBasica :: Pizza
pizzaDeAnchoasBasica = Pizza {
    ingredientes = ["salsa", "anchoas"],
    tamanio = 8,
    calorias = 270
}

--6.d
pizzeriaGourmet :: Number -> Pizzeria
pizzeriaGourmet nivelExquisitez =
    map agrandar . filter (not.deplorable) 
    where deplorable pizza = nivelDeSatisfaccion pizza <= nivelExquisitez
pizzeriaLaJauja = pizzeriaGourmet 399

--7.a
sonDignasDeCalleCorrientes :: Pedido -> [Pizzeria] -> [Pizzeria]
sonDignasDeCalleCorrientes pedido pizzerias =
    filter (mejoraSatisfaccion pedido) pizzerias
    where mejoraSatisfaccion pedido pizzeria =
            nivelDeSatisfaccionPedido pedido < (nivelDeSatisfaccionPedido . pizzeria) pedido

--7.b
mejorPizzeria :: Pedido -> [Pizzeria] -> Pizzeria
mejorPizzeria pedido = maximoSegun (nivelDeSatisfaccionPedido . ($ pedido)) 

maximoSegun f = foldl1 (mayorSegun f)
    where mayorSegun f a b
            | f a > f b = a
            | otherwise = b

--8
--Teorico

--9
laPizzeriaPredilecta :: [Pizzeria] -> Pizzeria
laPizzeriaPredilecta = foldl1 (.)


--Huber

--data Ubicacion = Intersección calle1 calle2 | Altura calle número
data Ubicacion = 
    Interseccion String String | 
    Altura String Number 
    deriving (Show, Eq)

data Persona = Persona {
    edad :: Number,
    tiempoLibre :: Number,
    dineroEncima :: Number
} deriving Show

huberto = Persona 42 10 10.2
leto = Persona 32 20 20

--colectivos = [(línea, paradas)]
colectivos :: [(Number, [Ubicacion])]
colectivos = [(14,[Interseccion "Salguero" "Lavalle", Interseccion "Salguero" "Potosi",
    Interseccion "Bulnes" "Potosi", Altura "Bulnes" 1200]), 
    (168,[Altura "Corrientes" 100, Altura "Corrientes" 300, Altura "Corrientes" 500, 
    Altura "Corrientes" 700])]

data Huber = Huber {
    origen :: Ubicacion,
    destino :: Ubicacion,
    importe :: Number
} deriving Show

hubers :: [Huber]
hubers = [Huber (Interseccion "Salguero" "Lavalle") (Interseccion "Bulnes" "Potosi") 5,
    Huber (Altura "Av Corrientes" 700) (Interseccion "Sarmiento" "Gallo") 10,
    Huber (Interseccion "Salguero" "Potosi") (Altura "Ecuador" 600) 20,
    Huber (Altura "Corrientes" 300) (Altura "Corrientes" 700) 5,
    Huber (Interseccion "Salguero" "Lavalle") (Altura "Bulnes" 1200) 5]

ubicaciones :: [Ubicacion]
ubicaciones = [ubicacion1, Interseccion "Salguero" "Potosi",
    ubicacion2, Interseccion "Sarmiento" "Gallo",
    Altura "Ecuador" 600, Altura "Corrientes" 100, Altura "Corrientes" 300, 
    Altura "Corrientes" 500, Altura "Corrientes" 700,ubicacion3]

ubicacion1 = Interseccion "Salguero" "Lavalle" 
ubicacion2 = Interseccion "Bulnes" "Potosi"
ubicacion3 = Altura "Bulnes" 1200

--estanOrdenadas/3
{-
dadas dos ubicaciones y una lista, determina si la primera ubicación (primer parámetro) aparece antes 
en la lista que la segunda ubicación (segundo parámetro). Si alguna de las dos ubicaciones no aparece en la lista, 
o si no respetan el orden, devolverá False.
-}
estanOrdenadas u1 u2 lista = indexOf u1 lista < indexOf u2 lista

indexOf e lista = recIndexOf 1 e lista 

recIndexOf _ _ [] = 0 -- es feo :)
recIndexOf index e (x:xs)
    | e==x = index
    | otherwise = recIndexOf (index+1) e xs

--1
importeEnHuber :: Ubicacion -> Ubicacion -> Number
importeEnHuber ubi1 ubi2 
    | any realizaViaje hubers = importe . head . filter realizaViaje $ hubers
    | otherwise = 0
    where realizaViaje huber = ubi1 == origen huber && ubi2 == destino huber

importeEnHuber' :: Ubicacion -> Ubicacion -> Number
importeEnHuber' ubi1 ubi2 
    | any (realizaViaje' ubi1 ubi2 ) hubers = importe . head . filter (realizaViaje' ubi1 ubi2) $ hubers
    | otherwise = 0

realizaViaje' :: Ubicacion -> Ubicacion -> Huber -> Bool
realizaViaje' ubi1 ubi2  huber = ubi1 == origen huber && ubi2 == destino huber

--2
sePuedeIrEnBondi :: Ubicacion -> Ubicacion -> Bool
sePuedeIrEnBondi ubi1 ubi2 = any (realizaViajeEnBondi ubi1 ubi2) colectivos

realizaViajeEnBondi :: Ubicacion -> Ubicacion -> (Number, [Ubicacion]) -> Bool
realizaViajeEnBondi ubi1 ubi2 = (estanOrdenadas ubi1 ubi2).snd

--3
masBaratasQueHuber :: Ubicacion -> Ubicacion -> [Number]
masBaratasQueHuber ubicacion1 ubicacion2 = 
    map fst . filter condicion $ colectivos
    where
        condicion colectivo =
            realizaViajeEnBondi ubicacion1 ubicacion2 colectivo &&
            importeEnColectivo ubicacion1 ubicacion2 colectivo < limite
        limite = importeEnHuber ubicacion1 ubicacion2

importeEnColectivo :: Ubicacion -> Ubicacion -> (Number, [Ubicacion]) -> Number
importeEnColectivo ubicacion1 ubicacion2 =
    (2*) . cantidadDeParadasEntre ubicacion1 ubicacion2 . snd 

cantidadDeParadasEntre :: Ubicacion -> Ubicacion -> [Ubicacion] -> Number
cantidadDeParadasEntre ubicacion1 ubicacion2 =
    length . tail . dropWhile (/= ubicacion1) . dropWhileEnd (/= ubicacion2)

dropWhileEnd condicion = reverse . dropWhile condicion . reverse

--4.a
distanciaEntre :: Ubicacion -> Ubicacion -> [Ubicacion] -> Number
distanciaEntre ubicacion1 ubicacion2 ubicaciones = 
    (4*).(cantidadDeParadasEntre ubicacion1 ubicacion2) $ ubicaciones


-- CurryCoin

--1.a
type Id = String
data Cuenta = Cuenta {
    idCuenta :: Id,
    saldo :: Number
} deriving (Show,Eq)

type Transaccion = Cuenta -> Cuenta
type Bloque = [(Id, Transaccion)]

--1.b
transaccion :: Number -> Transaccion
transaccion numero cuenta = cuenta{ saldo = numero + saldo cuenta}

pago :: Number -> Transaccion
pago numero = transaccion (-numero)

cobro :: Number -> Transaccion
cobro = transaccion

mineria :: Transaccion
mineria = transaccion 25

--2.a
correspondeId :: Id -> Cuenta -> Bool
correspondeId identificador = (identificador==).idCuenta 

--2.b
primeraQueCumple :: (Cuenta -> Bool) -> [Cuenta] -> Cuenta
primeraQueCumple condicion cuentas = head . filter condicion $ cuentas

--2.c
sinLaPrimeraQueCumple :: (Cuenta -> Bool) -> [Cuenta] -> [Cuenta]
sinLaPrimeraQueCumple condicion cuentas = (takeWhile (/= primera) cuentas ++).tail.(dropWhile (/= primera)) $ cuentas
    where primera = primeraQueCumple condicion cuentas

--3
modificarCuenta :: Id -> [Cuenta] -> Transaccion -> [Cuenta]
modificarCuenta identificador cuentas transax = transax (primeraQueCumple esId cuentas) : sinLaPrimeraQueCumple esId cuentas
    where esId = correspondeId identificador

--4
ejecutarTransacciones :: Bloque -> [Cuenta] -> [Cuenta]
ejecutarTransacciones bloque cuentas = foldl aplicarTransax cuentas bloque
    where
        aplicarTransax :: [Cuenta] -> (Id, Transaccion) -> [Cuenta]
        aplicarTransax cuentas (identificador, transax) = modificarCuenta identificador cuentas transax

--5
sonEstables :: [Cuenta] -> Bool
sonEstables = all ((0 <=).saldo)

--6
type Blockchain = [Bloque]
{-
intermediosEstables :: Blockchain -> [Cuenta] -> Bool
intermediosEstables blockchain cuentas =
  sonEstables $ foldl (flip ejecutarTransacciones) cuentas (concat blockchain)
-}
blockchainEstable' :: Blockchain -> [Cuenta] -> Bool
blockchainEstable' [] _ = True
blockchainEstable' (x:xs) cuentas
    | sonEstables (ejecutarTransacciones x cuentas) = blockchainEstable' xs cuentas
    | otherwise = False

--7
funcionSinPudor :: [[a]] -> (a -> Number, (b -> b)) -> (b -> b)
funcionSinPudor x y 
  | (length . filter even . map (fst y) $ head x) > 10 = id
  | otherwise = snd y


--Lovecraft

--Modelo Inicial
data Investigador = Investigador{
    nombreInv :: String,
    cordura :: Number,
    items :: [Item],
    sucesosEvitados :: [String]
} deriving (Show, Eq)

data Item = Item{
    nombreItem :: String,
    valor :: Number
} deriving (Show, Eq)

--Funciones Útiles
{-
maximoSegun f = foldl1 (mayorSegun f)
    where mayorSegun f a b
            | f a > f b = a
            | otherwise = b
-}

deltaSegun ponderacion transformacion valor =
    abs ((ponderacion . transformacion) valor - ponderacion valor)

type CambioEstado = Investigador -> Investigador
type CambioEstadoGrupal = Grupo -> Grupo

pepe :: Investigador
pepe = Investigador "Pepe" 10 [dagaMaldita] ["Despertar de un antiguo"]

luis :: Investigador
luis = Investigador "Luis" 2 [necronomicon] ["Ritual en Innsmouth"]

necronomicon :: Item
necronomicon = Item "Necronomicon" 3

dagaMaldita :: Item
dagaMaldita = Item "Daga maldita" 3

--1.a
enloquecer :: Number -> Investigador -> Investigador
enloquecer numero investigador = investigador{cordura = locura numero investigador}

locura :: Number -> Investigador -> Number
locura numero investigador = max 0 (cordura investigador - numero)

--1.b
hallarItem :: Item -> Investigador -> Investigador
hallarItem item investigador = investigador{
    cordura = locura (valor item) investigador,
    items = (item : items investigador)
    }

--2
type Grupo = [Investigador]

tieneItem :: Item -> Grupo -> Bool
tieneItem item = any (f item)
    where
        f :: Item -> Investigador -> Bool
        f item = (elem item).items

--3
lider :: Grupo -> Investigador
lider = maximoSegun potencial

potencial :: Investigador -> Number
potencial investigador
    | (0 ==).cordura $ investigador = 0
    | otherwise = ((cordura investigador * ).experiencia $ investigador) + (calcularMaximoValorItem investigador)
       where calcularMaximoValorItem investigador = valor (maximoSegun valor (items investigador))

experiencia :: Investigador -> Number
experiencia = ((1+ ).(3* ).length.sucesosEvitados)

--4.a
corduraTotal :: Number -> Grupo -> Number
corduraTotal numero = sum.(map (deltaSegun cordura (enloquecer numero)))

--4.b
primerIntegrante :: Grupo -> Number
primerIntegrante = deltaSegun (potencial.head) (filter ((0 ==).cordura))

--4.c
--En ambos casos sería imposible; en el caso de a, la sumatoria tendería
--a infinito, por lo que no podría terminar de sumarlos. Por el otro lado,
--en el caso de b, nunca terminaría de filtrar a los investigadores locos.


-- Samurai Jack

-- Enunciado: https://docs.google.com/document/d/1mhQ2R8VjpoVrQ5JroYbkiBHjZME9gw6jEn6OI2q6Q2U

data Personaje = UnPersonaje {
    nombrePersonaje :: String,
    salud :: Number,
    elementos :: [Elemento],
    anioPresente :: Number
  } deriving (Show, Eq)

data Elemento = UnElemento {
  tipo :: String,
  ataque :: (Personaje-> Personaje),
  defensa :: (Personaje-> Personaje)
}

instance Show Elemento where
  show = tipo

instance Eq Elemento where
  (==) elemento1 elemento2 = tipo elemento1 == tipo elemento2

-- Punto 1
mandarAlAnio :: Number -> Personaje -> Personaje
mandarAlAnio anio (UnPersonaje nombrePersonaje salud elementos _) =
  UnPersonaje nombrePersonaje salud elementos anio

-- Equivalente con azúcar sintáctico que nos aporta el data definido como lo hacemos normalmente:
-- <DISCLAIMER>
-- sólo usar este azúcar sintáctico en funciones muy chiquititas
-- que sólo hagan un cambio a la vez y puedan ser fácilmente reutilizadas y combinadas entre sí
-- No usar esto por todos lados (de la misma forma que no está bueno
-- usar pattern matching por todos lados)
mandarAlAnio' :: Number -> Personaje -> Personaje
mandarAlAnio' anio personaje = personaje { anioPresente = anio }
-- Esta sintaxis tiene la ventaja de que es menos verbosa y que se ve menos afectada
-- por cambios estructurales, pero no es una función, y en funcional queremos trabajar con funciones!
-- El abuso de esta notación lleva a no explotar los conceptos ricos de funcional que nos interesan
-- que son orden superior, aplicación parcial y composición. Si no saben cuando detenerse, no lo usen.
-- </DISCLAIMER>

meditar :: Personaje -> Personaje
meditar = modificarSalud (*1.5)

causarDanio :: Number -> Personaje -> Personaje
causarDanio danio = modificarSalud (max 0 . flip (-) danio)

modificarSalud :: (Number -> Number) -> Personaje -> Personaje
-- Abstracción útil, super reutilizable :D
-- Acá vale usar el azúcar sintáctico o el equivalente con pattern matching
-- Una solución que no abstrae esta idea, indefectiblemente repite lógica
-- entre meditar y causarDanio con o sin sintaxis cheta
modificarSalud transformacion personaje =
  personaje { salud = (transformacion . salud) personaje }

-- Punto 2
esMalvado :: Personaje -> Bool
esMalvado personaje =
  (any (esDeTipo "Maldad") . elementos) personaje

esDeTipo unTipo elemento = tipo elemento == unTipo

danioQueProduce :: Personaje -> Elemento -> Number
danioQueProduce personaje elemento =
  salud personaje - salud (ataque elemento personaje)

danioQueProduce' personaje elemento =
  ((salud personaje -) . salud . ataque elemento) personaje

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje enemigos =
  filter (esEnemigoMortal personaje) enemigos

esEnemigoMortal personaje enemigo =
  (any (tieneAtaqueMortal personaje) . elementos) enemigo

tieneAtaqueMortal personaje elemento =
  danioQueProduce personaje elemento == salud personaje

tieneAtaqueMortal' personaje elemento =
  (estaMuerto . ataque elemento) personaje

estaMuerto  = ((==0).salud)

-- Punto 3
noHacerNada = id

concentracion :: Number -> Elemento
concentracion nivelDeConcentracion =
  UnElemento { tipo = "Magia",
               ataque = noHacerNada,
               defensa = (!! nivelDeConcentracion) . iterate meditar }
               -- equivalente con composición y aplicación parcial para:
               -- defensa = (\personaje -> iterate meditar personaje !! nivelDeConcentracion) }
               -- otra versión super interesante:
               -- defensa = foldr1 (.) (replicate nivelDeConcentracion meditar)
               -- por ejemplo (concentracion 3) resultaría en meditar.meditar.meditar

esbirrosMalvados :: Number -> [Elemento]
esbirrosMalvados cantidad = replicate cantidad unEsbirro

unEsbirro :: Elemento
unEsbirro = UnElemento "Maldad" (causarDanio 1) noHacerNada

jack :: Personaje
jack = UnPersonaje {
  nombrePersonaje = "Jack",
  salud = 300,
  elementos = [concentracion 3, katanaMagica],
  anioPresente = 200
}

katanaMagica = UnElemento "Magia" (causarDanio 1000) noHacerNada

aku :: Number -> Number -> Personaje
-- Este lo salteamos en clase por falta de tiempo, agrego una solución
-- Como se puede ver aku es recursivo porque tiene portalAlFuturoDesde como elemento
-- cuya defensa es generar un nuevo aku en otro año
aku anio saludInicial = UnPersonaje {
  nombrePersonaje = "Aku",
  salud = saludInicial,
  anioPresente = anio,
  elementos = concentracion 4 : portalAlFuturoDesde anio : esbirrosMalvados (100 * anio)
}
portalAlFuturoDesde anio = UnElemento "Magia" (mandarAlAnio anioFuturo) (aku anioFuturo.salud)
  where anioFuturo = anio + 2800

-- Punto 4
luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor
 |estaMuerto atacante = (defensor, atacante)
 |otherwise = luchar proximoAtacante proximoDefensor
 where proximoAtacante = usarElementos ataque defensor (elementos atacante)
       proximoDefensor = usarElementos defensa atacante (elementos atacante)

-- Abstraemos cómo hacer para usar uno de los efectos de un conjunto de elementos sobre un personaje
usarElementos :: (Elemento -> Personaje -> Personaje) -> Personaje -> [Elemento] -> Personaje
usarElementos funcion personaje elementos = foldl afectar personaje (map funcion elementos)

afectar personaje funcion = funcion personaje
afectar' = flip ($)

{-- Punto 5 (inferencia)
f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))

f :: (Eq t1, Numb t2) =>
     (t1 -> a1 -> (a2, a2)) -> (t2 -> t1) -> t1 -> [a1] -> [a2]
-}
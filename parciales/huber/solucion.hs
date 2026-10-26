module Library where
import PdePreludat

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
{-dadas dos ubicaciones devuelva el precio de ir desde la primera ubicación hacia la segunda. 
Si no hay algún huber que realice dicho viaje, entonces devolver cero-}
importeEnHuber ubicacion1 ubicacion2 
    | any realizaViaje hubers = importe . head . filter realizaViaje $ hubers
    | otherwise               = 0
    where realizaViaje huber = ubicacion1 == origen huber && ubicacion2 == destino huber

importeEnHuber' ubicacion1 ubicacion2 
    | any (realizaViaje' ubicacion1 ubicacion2) hubers = importe . head . filter (realizaViaje' ubicacion1 ubicacion2) $ hubers
    | otherwise               = 0

realizaViaje' ubicacion1 ubicacion2 huber = ubicacion1 == origen huber && ubicacion2 == destino huber

--2
{-dadas dos ubicaciones, determine si existe una línea que vaya de la primera ubicación a la segunda-}
sePuedeIrEnBondi ubicacion1 ubicacion2 =
    any (realizaViajeBondi ubicacion1 ubicacion2) colectivos

realizaViajeBondi ubicacion1 ubicacion2 = estanOrdenadas ubicacion1 ubicacion2 . snd

--3
{-devuelve las líneas de colectivos cuyo viaje desde la primera ubicación hacia la segunda ubicación sea más barato que el mismo viaje en un Huber-}
masBaratasQueHuber ubicacion1 ubicacion2 = 
    map fst . filter condicion $ colectivos
    where
        condicion colectivo =
            realizaViajeBondi ubicacion1 ubicacion2 colectivo &&
            importeEnColectivo ubicacion1 ubicacion2 colectivo < limite
        limite = importeEnHuber ubicacion1 ubicacion2

importeEnColectivo ubicacion1 ubicacion2 =
    (2*) . cantidadDeParadasEntre ubicacion1 ubicacion2 . snd 

cantidadDeParadasEntre ubicacion1 ubicacion2 =
    length . tail . dropWhile (/= ubicacion1) . dropWhileEnd (/= ubicacion2)

dropWhileEnd condicion = reverse . dropWhile condicion . reverse

--4
distanciaEntre ubicacion1 ubicacion2 ubicaciones = 
    cantidadDeParadasEntre ubicacion1 ubicacion2 ubicaciones * 4







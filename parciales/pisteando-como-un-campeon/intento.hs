module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

{-
    Nombre: Marco Szapari Marcos
    Legajo: 2092025
-}

--Pisteando como un campeón

--1
data Auto = Auto{
    marca :: String,
    modelo :: String,
    desgaste :: (Number, Number), --(Ruedas, Chasis)
    velocidadMaxima :: Number,
    tiempo :: Number
} deriving Show

--1.a
ferrari :: Auto
ferrari = Auto{
    marca = "Ferrari",
    modelo = "F50",
    desgaste = (0, 0),
    velocidadMaxima = 65,
    tiempo = 0
}

--1.b
lamborghini :: Auto
lamborghini = Auto{
    marca = "Lamborghini",
    modelo = "Diablo",
    desgaste = (4, 7),
    velocidadMaxima = 73,
    tiempo = 0
}

--1.c
fiat :: Auto
fiat = Auto{
    marca = "Fiat",
    modelo = "600",
    desgaste = (27, 33),
    velocidadMaxima = 44,
    tiempo = 0
}

--2.a
buenEstado :: Auto -> Bool
buenEstado auto = ((fst.desgaste $ auto) < 60 && ).(< 40).snd.desgaste $ auto

--2.b
noDaMas :: Auto -> Bool
noDaMas auto = ((fst.desgaste $ auto) > 80 || ).(> 80).snd.desgaste $ auto

--3
repararUnAuto :: Auto -> Auto
repararUnAuto auto = auto{desgaste = (0, ((snd.desgaste $ auto) -).(* 0.85).snd.desgaste $ auto)}

--4
type Tramo = Auto -> Auto

data Tramo' = Tramo'{
    longitud :: Number,
    tiempoTramo :: Number -> Number -> Auto -> Number
}

type Pista = [Tramo]
type Pista' = [Tramo']

--4.a.i
curva :: Number -> Number -> Tramo
curva angulo longitud auto = auto{
    desgaste = (((fst.desgaste $ auto) -).(1/).(angulo * 3 *) $ longitud, snd.desgaste $ auto),
    tiempo = tiempoCurva longitud auto}

tiempoCurva :: Number -> Auto -> Number
tiempoCurva longitud auto = (/2).(tiempoRecta longitud) $ auto

--4.a.ii
curvaPeligrosa :: Tramo
curvaPeligrosa = curva 60 300

--4.a.iii
curvaTranca :: Tramo
curvaTranca = curva 110 550

--4.b.i
recta :: Number -> Tramo
recta longitud auto = auto{
    desgaste = (fst.desgaste $ auto, ((snd.desgaste $ auto) -).(/100) $ longitud),
    tiempo = tiempoRecta longitud auto}
    
tiempoRecta :: Number -> Auto -> Number
tiempoRecta longitud auto = (tiempo auto +).(longitud /).( /2).velocidadMaxima $ auto

--4.b.ii
tramoRectoClassic :: Tramo
tramoRectoClassic = recta 750

--4.b.iii
tramito :: Tramo
tramito = recta 280

--4.c
box :: Tramo
box auto
    | buenEstado auto = tiempo tramo?
    | otherwise = auto{tiempo = (10 +).tiempo $ auto}

--4.d
tramoMojado :: Tramo
estaMojada auto = auto{tiempo = (((/2).tiempo $ tramo) +).tiempo $ auto}

--4.e
ripio :: Tramo
ripio = (tramo.tramo)

--4.f
tramoObstruido :: Number -> Tramo
tramoObstruido longitudObstruccion auto = auto{desgaste = (((fst.desgaste $ auto) -).(2*) $ longitudObstruccion,snd.desgaste $ auto)}

--5
pasarPorTramo :: Tramo -> Auto -> Auto
pasarPorTramo tramo auto
    | not.noDaMas $ auto = tramo auto
    | otherwise = auto

--6.a
superPista :: Pista
superPista pista = [tramoRectoClassic, curvaTranca, tramito.tramoMojado.tramito, (tramoObstruido 2).(curva 80 400),
(curva 115 650), (recta 970), curvaPeligrosa, ripio.tramito, box.(recta 800)]

--6.b
peganLaVuelta :: Pista -> [Auto] -> [Auto]
peganLaVuelta pista autos

--7.a
data Carrera = Carrera{
    pista :: Pista,
    vueltas :: Number
}

--7.b
tourBuenosAires :: Carrera
tourBuenosAires carrera = carrera{
    pista = superPista,
    vueltas = 20
}

--7.c
jugarCarrera :: Carrera -> [Auto] -> [Auto]
jugarCarrera carrera autos =
--Tengo que aplicar todas las funciones de la pista, "vueltas" veces a cada auto de la lista, teniendo en cuenta
--que se eliminan de la lista aquellos que no dan más.

--8
myPrecious :: [(a, b -> Number)] -> ((a, b -> Number) -> [c], b) -> Number
myPrecious ts x = sum . map (($ snd x) . snd) . filter ((>5) . length . fst x) $ ts
{-
Primero, la función devuelve un Number porque la última función que utiliza es sum. Luego, x debe ser una tupla de
dos elementos porque se utilizan fst y snd sobre ella. Su primer elemento debe ser una función, debido a que se
puede utilizar en una composición (y a su vez, esta función debe devolver una lista porque después se le aplica la
función length). Después, ts debe ser una lista porque se utiliza filter sobre ella; la misma debe contener una
tupla, porque se le aplica la función snd a cada elemento. Luego, el segundo elemento debe ser una función que reciba
el tipo de dato "snd x" y devuelva Number para poder interactuar con sum y devolver el resultado final.
-}

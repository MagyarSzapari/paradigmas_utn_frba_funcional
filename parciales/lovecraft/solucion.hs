module Library where
import PdePreludat

-- Lovecraft

--Modelo Inicial
data Investigador = Investigador{
    nombre :: String,
    cordura :: Number,
    items :: [Item],
    sucesosEvitados :: [String]
} deriving Show

data Item = Item{
    nombreItem :: String,
    valor :: Number
} deriving Show

--Funciones Útiles
maximoSegun f = foldl1 (mayorSegun f)
    where mayorSegun f a b
        | f a > f b = a
        | otherwise = b

deltaSegun ponderacion transformacion valor =
    abs ((ponderacion . transformacion) valor - ponderacion valor)

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
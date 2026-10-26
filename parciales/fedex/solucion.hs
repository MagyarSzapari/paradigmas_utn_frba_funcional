module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Envio = Envio {
    origen :: Locacion,
    destino :: Locacion,
    peso :: Number,
    precioBase :: Number,
    categorias :: [Categoria],
    impuesto :: [Impuesto]
} deriving Show

type Categoria = String

data Locacion = Locacion {
    ciudad :: String,
    pais :: String
} deriving (Show, Eq)


--1.a
-- type Cargo = Envio -> Number
type Cargo = Envio -> Envio
--1.b
type Impuesto = Envio -> Number


--2.a
cargoTecnologia = cargoCategorico "tecnologia" 18

cargoCategorico categoria porcentaje envio = 
    aplicarCargo (elem categoria . categorias) (precioBase envio * porcentaje/100) envio
{-cargoCategorico categoria porcentaje envio
    | elem categoria . categorias $ envio  = envio{ precioBase = precioBase envio * (1 + porcentaje/100) }
    | otherwise = envio
-}

cargoSobrepeso pesoLimite envio --TODO
    | pesoLimite >= peso envio = envio
    | otherwise = envio{ precioBase = precioBase envio + (peso envio - pesoLimite) * 80}


aplicarCargo condicion monto envio 
    | condicion envio = envio{ precioBase = precioBase envio + monto}
    | otherwise = envio

aplicarCargo' condicion funcionMonto envio 
    | condicion envio = envio{ precioBase = funcionMonto.precioBase $ envio}
    | otherwise = envio

--2.b
--envioInternacional
--envioLocal

--3
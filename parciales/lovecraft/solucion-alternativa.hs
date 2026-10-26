{-
Nombre: Vazquez, Lucas
Legajo: 209224-4
-}

module Lib where
import Text.Show.Functions

----------------------------------------------
-- Código base provisto en el enunciado
----------------------------------------------

data Investigador = Investigador {
  nombre :: String,
  cordura :: Int, 
  items :: [Item],
  sucesosEvitados :: [String]
} deriving (Show, Eq)

data Item = Item {
  nombreItem :: String,
  valor :: Int
} deriving (Show, Eq)


maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

deltaSegun ponderacion transformacion valor =
  abs ((ponderacion . transformacion) valor - ponderacion valor)

type CambioEstado = Investigador->Investigador
type CambioEstadoGrupal = [Investigador]->[Investigador]

pepe::Investigador
pepe = Investigador "Pepe" 10 [dagaMaldita] ["Despertar de un antiguo"]

luis::Investigador
luis = Investigador "Luis" 2 [necronomicon] ["Ritual en Innsmouth"]

necronomicon::Item
necronomicon = Item "Necronomicon" 3

dagaMaldita::Item
dagaMaldita = Item "Daga maldita" 3
----------------------------------------------
-- Definí tus tipos de datos y funciones aquí
-- abajo, indicando a qué punto pertenecen
----------------------------------------------

--Punto 1

enloquecer:: Int->CambioEstado
enloquecer puntos (Investigador nombreInv corduraActual itemsInv sucesos) = Investigador nombreInv (max 0 (corduraActual-puntos)) itemsInv sucesos

hallarItem::Item->CambioEstado
hallarItem itemHallado =  incorporarItem itemHallado . enloquecer (valor itemHallado) 

incorporarItem::Item->CambioEstado
incorporarItem item (Investigador nombreInv corduraActual itemsInv sucesos) = Investigador nombreInv corduraActual (item : itemsInv) sucesos

--Punto 2
elGrupoTieneElItem::String->[Investigador]->Bool
elGrupoTieneElItem nomItem = any (tieneElItem nomItem)

tieneElItem::String->Investigador->Bool
tieneElItem nomItem = elem nomItem . map nombreItem . items

--Punto 3
cualEsLider::[Investigador]->Investigador
cualEsLider = maximoSegun calcularPotencial

calcularPotencial::Investigador->Int
calcularPotencial investigador 
   | (not .estaCuerdo) investigador = 0
   | estaCuerdo investigador = ((+) (valorMaximoItem (items investigador)) . (*) (calcularExperiencia investigador) . cordura )investigador

estaCuerdo::Investigador->Bool
estaCuerdo investigador 
   |cordura investigador == 0 = False
   |otherwise = True

calcularExperiencia::Investigador->Int
calcularExperiencia = (+) 1 . length . sucesosEvitados

valorMaximoItem::[Item]->Int
valorMaximoItem = valor . maximoSegun (valor)

--Punto 4
--a
--Suma total de las diferencias de cordura de cada investigador
deltaCordura::Int->[Investigador]->Int
deltaCordura puntos = sum . map (deltaSegun cordura (enloquecer puntos))

--Diferencia de potencial entre el primero Original y el primero luego de perder a los locos
deltaPotencialPrimero::[Investigador]->Int
deltaPotencialPrimero investigadores =  ((-) (calcularPotencial $ head (investigadores)) . calcularPotencial.head . filter estaCuerdo) investigadores

--c Para el caso a) no seria posible ya que al ser una lista infinita tendriamos que consultar infinitos investigadores
--para poder obtener un delta total. En pocas palabras es sumar inifnitimanete lo cual nunca acaba.
-- Para el caso b) no hay que sumar infinitos elementos, pero si hay que filtrar una lista infinita que tampoco
-- podremos acabar de filtrar ya que nunca se acaban los elementos que tenemos que consultar

--5
data Suceso = Suceso {
  descripcion::String,
  consecuencias :: [CambioEstadoGrupal],
  condicionEvitarlo :: [Investigador]->Bool
}deriving (Show)


despertarAntiguo::Suceso
despertarAntiguo = Suceso "Despertar de un antiguo" [enloquecerTodos 10 , tail ] (elGrupoTieneElItem "Necronomicon")

enloquecerTodos::Int->CambioEstadoGrupal
enloquecerTodos puntos = map (enloquecer puntos)

ritualInnsmouth::Suceso
ritualInnsmouth = Suceso "Ritual en Innsmouth" [primeroHallaItem dagaMaldita , enloquecerTodos 2]  ((<) 100 . calcularPotencial.cualEsLider)

primeroHallaItem::Item->CambioEstadoGrupal
primeroHallaItem item investigadores = (flip (:) (tail investigadores) . hallarItem item  . head) investigadores

enfrentarSuceso::Suceso->CambioEstadoGrupal
enfrentarSuceso suceso investigadores
   | condicionEvitarlo suceso investigadores = (agregarSuceso suceso . enloquecerTodos 1) investigadores
   | (not . condicionEvitarlo suceso) investigadores = 
      (enloquecerTodos 1 . ejecutarConsecuencias (consecuencias suceso)) investigadores

ejecutarConsecuencias::[CambioEstadoGrupal]->CambioEstadoGrupal
ejecutarConsecuencias consecuencias investigadores = foldr ($) investigadores consecuencias

agregarSuceso::Suceso->CambioEstadoGrupal
agregarSuceso suceso investigadores = map (agregarDescripcionSuceso suceso) investigadores

agregarDescripcionSuceso::Suceso->CambioEstado
agregarDescripcionSuceso suceso (Investigador nombreInv corduraActual itemsInv sucesosAnteriores) = 
  Investigador nombreInv corduraActual itemsInv ((descripcion suceso) : sucesosAnteriores)

sucesoMasAterrador::[Investigador]->[Suceso]->Suceso
sucesoMasAterrador investigadores = maximoSegun (flip calcularTerror investigadores) 

calcularTerror::Suceso->[Investigador]->Int
calcularTerror suceso =  deltaSegun (sum . map cordura) (enfrentarSuceso suceso) 

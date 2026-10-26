module Library where
import PdePreludat

{-
Nombre y Apellido: Marco Szapari Marcos
Legajo: 209202-5
-}

doble :: Number -> Number
doble numero = numero + numero

--Punto 1
data Pizza = Pizza{
    ingredientes :: [String],
    tamanio :: Number,
    calorias :: Number
}

grandeDeMuzza :: Pizza
grandeDeMuzza = pizza{ingredientes = ["salsa", "mozzarella", "orégano"], tamanio = 8, calorias = 350}

--Punto 2
nivelDeSatisfaccion :: Pizza -> Number
nivelDeSatisfaccion pizza
    | elem "palmito" (ingredientes pizza) = 0
    | calorias pizza < 500 = (80* ).(length.ingredientes) $ pizza
    | otherwise = (40* ).(length.ingredientes) $ pizza

--Punto 3
precioDePizza :: Pizza -> Number
precioDePizza pizza = ((120* ).(length.ingredientes) $ pizza) * tamanio pizza

--Punto 4
nuevoIngrediente :: String -> Pizza -> Pizza
nuevoIngrediente ingrediente pizza = pizza{ingredientes = ingredientes pizza ++ [ingrediente], calorias = (calorias pizza +).(2*).length $ ingrediente}

agrandar :: Pizza -> Pizza
agrandar pizza
    | tamanio pizza == 10 = pizza{tamanio = 10}
    | otherwise = pizza{tamanio = (2+ ).tamanio $ pizza}

mezcladita :: Pizza -> Pizza -> Pizza
mezcladita pizza1 pizza2 = pizza2{ingredientes = (ingredientes pizza2 ++).ingredientes $ pizza1, calorias = (calorias pizza2 +).(1/2 *).calorias $ pizza1}

--Punto 5
type Pedido = [Pizza]

--satisfaccionDeUnPedido :: Pedido -> [Number]
--satisfaccionDeUnPedido pedido = foldl1 (+) (map nivelDeSatisfaccion pedido)

pizzeriaLosHijosDePato :: Pedido -> Pedido
pizzeriaLosHijosDePato  = map (nuevoIngrediente "Palmito")

--pizzeriaElResumen :: Pedido -> Pedido
--zip

pizzeriaEspecial :: Pizza -> Pedido -> Pedido
pizzeriaEspecial pizzaPredilecta = map (mezcladita pizzaPredilecta)

pizzeriaPescadito :: Pedido -> Pedido
pizzeriaPescadito = pizzeriaEspecial (pizza{ingredientes = ["salsa", "anchoas"], tamanio = 8, calorias = 270})

pizzeriaGourmet :: Pedido -> Pedido

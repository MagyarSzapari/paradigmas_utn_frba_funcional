# Refactor del TP y de los parciales (v2.0)

Este documento cubre la reingeniería del **TP "Chicas Superpoderosas"** y de los **parciales de práctica** del paradigma funcional. Las guías de ejercicios se documentan aparte, en `docs/REFACTOR-guias.md`.

- Código original (sin tocar): `tp-chicas-superpoderosas/` y `parciales/`.
- Código refactorizado: `refactored/` (proyecto Stack, Prelude estándar, `-Wall` limpio, tests con HSpec + QuickCheck).
- Convención: **código en inglés** (identificadores, tipos, comentarios y datos de ejemplo) y **documentación en español**.

---

## 1. Executive Summary & Functional Diagnostic

### 1.1 Resumen

| Métrica | Original | Refactor |
|---|---|---|
| Dependencia de prelude | `PdePreludat` (tipo `Number`) | Prelude estándar (`Int`, `Double`, `Float`) |
| Archivos que compilan juntos | 1 proyecto (TP); los parciales son sueltos y algunos no compilan | 9 módulos en un solo paquete, todos compilan |
| Firmas de tipo explícitas | parciales (muchas funciones sin firma) | 100% de las funciones top-level y de los `where` relevantes |
| Advertencias con `-Wall` | no medido (PdePreludat las oculta con pragmas) | 0 |
| Funciones parciales evitables (`head`, `tail`, `foldl1`, `!!` sobre listas posiblemente vacías) | 19 usos (algunos protegidos por una guarda previa) | 1, documentado a propósito (`gongNeng`, ejercicio de inferencia) |
| Tests | 37 del TP (dos de ellos testeaban a la chica equivocada) | 194 ejemplos (TP + 8 módulos de parciales), incluidas 28 propiedades QuickCheck |

### 1.2 Diagnóstico del código original

El código original ya tenía estilo funcional (composición, aplicación parcial, `map`/`filter`/`fold`). Los problemas eran de **robustez**, **modelado** y **consistencia**, no de paradigma:

1. **Funciones parciales.** `head . filter`, `tail`, `foldl1` y `(!! n)` revientan con listas vacías o índices negativos. Ejemplos: `primeraQueCumple` (CurryCoin), `choferConMenosViajes` (Huber v2, además con patrón incompleto para `[]`), `maximoSegun` en Lovecraft y Pizza, `laPizzeriaPredilecta = foldl1 (.)`, `distanciaEntre` en Huber.
2. **Tipos poco expresivos.** Tuplas anónimas para datos con significado (`desgaste :: (Number, Number)` en Pisteando, `fecha :: (Number, Number, Number)` en Huber v2, `colectivos :: [(Number, [Ubicacion])]`), y `String` para conjuntos cerrados de valores (`tipo = "Maldad"` en Samurai Jack).
3. **Estado global implícito.** En Huber, `importeEnHuber` y `sePuedeIrEnBondi` leen las constantes globales `hubers` y `colectivos`: la función sólo es testeable con *esos* datos.
4. **Duplicación.** CurryCoin define dos veces `Blockchain` y tiene `afectar` duplicando a `ejecutarTransacciones` (el archivo **no compila**); FedEx tiene `aplicarCargo` y `aplicarCargo'` casi iguales; Huber v1 tiene `importeEnHuber` e `importeEnHuber'`.
5. **Errores de precedencia escondidos por las secciones.** Cadenas como `(2 +).(calorias pizza *).length $ ingrediente` se leen de derecha a izquierda y es fácil equivocar la fórmula (ver bugs en 1.4).
6. **Intentos incompletos.** `pisteando-como-un-campeon/intento.hs` no compila (`tiempo tramo?`, funciones sin cuerpo, signos invertidos en el desgaste).

**Nivel de abstracción alcanzado en el refactor:** se mantiene (y se explicita) el estilo declarativo de la cátedra, pero con funciones **totales** (`Maybe` donde puede no haber resultado), **ADTs** para el dominio, **combinadores de orden superior** reutilizables (`modifyResistance`, `chargeWhen`, `updateFirst`, `applyTimes`, decoradores de tramos) y **dependencias explícitas** (los catálogos se pasan por parámetro).

### 1.3 Mapeo archivo original → módulo refactorizado

| Original | Módulo refactorizado | Tests |
|---|---|---|
| `tp-chicas-superpoderosas/src/Library.hs` (+ `historial/Library-entrega-1.hs`) | `refactored/src/PowerpuffGirls.hs` | `refactored/test/PowerpuffGirlsSpec.hs` |
| `tp-chicas-superpoderosas/src/Spec.hs` | (portado a) `refactored/test/PowerpuffGirlsSpec.hs` | — |
| `parciales/currycoin/solucion.hs` | `refactored/src/Exams/CurryCoin.hs` | `test/Exams/CurryCoinSpec.hs` |
| `parciales/fedex/solucion.hs` | `refactored/src/Exams/FedEx.hs` | `test/Exams/FedExSpec.hs` |
| `parciales/huber/solucion.hs` (enunciado "colectivos vs. Huber") | `refactored/src/Exams/Huber/BusRoutes.hs` | `test/Exams/Huber/BusRoutesSpec.hs` |
| `parciales/huber/solucion-v2.hs` (enunciado "choferes y viajes", **otro** parcial) | `refactored/src/Exams/Huber/Drivers.hs` | `test/Exams/Huber/DriversSpec.hs` |
| `parciales/lovecraft/solucion.hs` + `solucion-alternativa.hs` (mismo enunciado) | `refactored/src/Exams/Lovecraft.hs` (consolidado) | `test/Exams/LovecraftSpec.hs` |
| `parciales/pizza-conmigo/solucion.hs` + `intento.hs` (mismo enunciado) | `refactored/src/Exams/PizzaConmigo.hs` (consolidado) | `test/Exams/PizzaConmigoSpec.hs` |
| `parciales/pisteando-como-un-campeon/intento.hs` | `refactored/src/Exams/Racing.hs` | `test/Exams/RacingSpec.hs` |
| `parciales/samurai-jack/solucion.hs` | `refactored/src/Exams/SamuraiJack.hs` | `test/Exams/SamuraiJackSpec.hs` |
| `parciales/recuperatorio-peleadores/enunciado.png` | fuera de alcance (no hay código) | — |

Traducciones de dominio más relevantes (los nombres propios del TP se pasaron a los de la versión en inglés de la serie):

| Original | Refactor |
|---|---|
| Bombón / Burbuja / Bellota | `blossom` / `bubbles` / `buttercup` |
| Señorita Belo / Señor Cerdo / Silico | `msBellum` / `mrPig` / `silico` |
| Saltadilla / Pueblo Paleta | `townsville` / `palletTown` |
| Princesa / Banda Gangrena | `princess` / `gangreneGang` |
| `capitulo`, `temporada`, `darlePlay`, `maraton` | `Episode`, `Season`, `play`, `marathon` |
| Pizzerías "Los Hijos de Pato", "El Resumen", "Pescadito", "La Jauja" | `palmHeartPizzeria`, `summaryPizzeria`, `fishyPizzeria`, `jaujaPizzeria` |
| `Tramo`, `Pista`, `Carrera`, `box`, `ripio`, `tramoMojado` | `Section`, `Track`, `Race`, `pitStop`, `gravel`, `wet` |

### 1.4 Bugs encontrados en los originales y cambios de comportamiento

| # | Archivo | Bug | Qué hace el refactor |
|---|---|---|---|
| 1 | TP `Library.hs` | `noEsDebilAKriptonita` busca `"kriptonita"` pero Banda Gangrena tiene `"kryptonita"`: la debilidad **nunca** se detecta. Banda Gangrena no es de nivel alto igual (tiene 3 debilidades), por eso los tests pasaban. | Una única constante `kryptonite` usada en los datos y en el chequeo. Test de regresión con una amenaza débil a kryptonita. |
| 2 | TP `Library.hs` | `hacerAmigos` no impide que una chica se agregue a sí misma (lo exige el enunciado) ni deduplica la "juntada". | `befriend` filtra a la propia chica y aplica `nub`. Test de regresión + propiedad de idempotencia. |
| 3 | TP `Spec.hs` | Los tests "burbuja/bellota se quedan sin resistencia" en realidad evalúan `beberSustanciaX bombon`. | Cada test usa la chica correcta. Se agregó el caso del enunciado "Burbuja toma cocucha y ya no tiene velocidad", que faltaba. |
| 4 | TP `Library.hs` | `consumirTodosLosAlimentos = foldr ($) chica alimentos` aplica los alimentos **del último al primero**. | `fedGirl` usa `foldl (flip ($))`: orden de la lista. Los casos de prueba no cambian (los alimentos usados conmutan). |
| 5 | TP `Library.hs` | `nombreEmpiezaConB = (=='b').head.nombre` explota con nombre vacío (y el vodka puede dejar el nombre vacío). | `isPrefixOf "b" . map toLower . name`: total y sin distinguir mayúsculas. |
| 6 | CurryCoin | `type Blockchain` declarado dos veces → **no compila**. `modificarCuenta` mueve la cuenta modificada al principio de la lista y explota (`head []`) si el id no existe. | Un solo `Blockchain`; `modifyAccount` conserva el orden y deja la lista igual ante un id desconocido. Se agregó `applyBlockchain` (el tipo estaba declarado pero sin uso). |
| 7 | Huber v1 | `indexOf` devuelve 0 si no encuentra el elemento, así que `estanOrdenadas ausente presente lista` da **True** (el enunciado pide False). | `stopsBetween` con `elemIndex` y `Maybe`; `areOrdered` es False si falta cualquiera de las dos. Test de regresión. |
| 8 | Huber v1 | `distanciaEntre` con ubicaciones en orden inverso (o ausentes) hace `tail []` y explota. | `distanceBetween :: ... -> Maybe Int`. |
| 9 | Huber v2 | `choferConMenosViajes []` no tiene patrón → error si ningún chofer acepta el viaje. | `leastBusy`/`assignTrip` devuelven `Maybe Driver`. |
| 10 | Lovecraft `solucion.hs` | 4.b filtra `((0 ==).cordura)`: se queda con los **locos**, al revés de lo pedido. | `filter isSane`, como en la solución alternativa. Test de regresión. |
| 11 | Lovecraft (ambas) | `potencial` revienta si el investigador no tiene ítems (`foldl1` sobre `[]`). | `maxItemValue = maximum . (0 :) . map value . items`. |
| 12 | Lovecraft (ambas) | Respuesta teórica 4.c: dicen que 4.b "nunca terminaría de filtrar" con una lista infinita. **Es incorrecto**: `head . filter p` sobre una lista infinita termina apenas encuentra el primer elemento que cumple (evaluación perezosa). Sólo diverge si ningún investigador está cuerdo. | Documentado y testeado con `cycle`. |
| 13 | Lovecraft (alternativa) | La experiencia se calcula `1 + length sucesos` (sin el factor 3 de la otra solución). | Se toma `1 + 3 * length`, que coincide con `solucion.hs`. |
| 14 | Pizza `solucion.hs` | `nuevoIngrediente`: `(2 +).(calorias pizza *).length` calcula `2 + calorías * letras`, no `calorías + 2 * letras`. | `calories pizza + 2 * fromIntegral (length ingredient)`. Test de regresión. |
| 15 | Pizza `solucion.hs` | `calculoCalorias`: `(calorias p1 /).(2 +).calorias $ p2` calcula `c1 / (2 + c2)`, no `c2 + c1 / 2`. | `calories second + calories first / 2`. |
| 16 | Pizza `intento.hs` | `"Palmito"` con mayúscula: la satisfacción nunca da 0. | Una constante `palmHeart` compartida. |
| 17 | Pizza `solucion.hs` | `laPizzeriaPredilecta = foldl1 (.)` explota con lista vacía. | `foldr (.) id`. |
| 18 | Pisteando `intento.hs` | No compila; en curvas y obstrucciones el desgaste se **resta** y la fórmula de curva es `1 / (3 * ángulo * longitud)`. | Reconstruido (ver 1.5). |
| 19 | Samurai Jack | `concentracion n` usa `(!! n) . iterate meditar`: falla con `n` negativo. | `applyTimes n f = foldr (.) id (replicate n f)`. |

**Cambios de comportamiento intencionales (no son bugs, son decisiones de API):** las búsquedas que pueden no tener resultado devuelven `Maybe` (`predominantThreat`, `firstMatching`, `leader`, `scariestEvent`, `bestPizzeria`, `assignTrip`, `distanceBetween`); los catálogos de Huber se reciben por parámetro; el capítulo guarda el **nombre** de la ciudad (como dice el enunciado) y no una `Ciudad`.

### 1.5 Supuestos (no hay enunciados de los parciales)

- **Pisteando:** se reconstruyeron las reglas a partir del intento y del dominio. Curva: desgaste de ruedas `3 * longitud / ángulo`, tiempo `longitud / (velocidadMáxima / 2)`. Recta: desgaste de chasis `longitud / 100`, tiempo `longitud / velocidadMáxima`. Boxes, mojado, ripio y obstrucción se modelan como **decoradores** (`Section -> Section`), que es lo que sugiere la superpista del intento (`ripio.tramito`, `box.(recta 800)`). "Dos tramitos, uno mojado" se interpretó como `[wet shortStraight, shortStraight]`. `jugarCarrera` devuelve el estado tras cada vuelta, sacando los autos que no dan más.
- **FedEx:** sólo estaban resueltos 1 y 2.a; 2.b se interpretó como "envío local / internacional" (mismo o distinto país).
- **CurryCoin:** puntos 5 a 7 estaban vacíos; sólo se agregó `applyBlockchain` porque el tipo ya estaba declarado.
- **Lovecraft:** la condición de evitar un suceso se evalúa sobre el grupo **antes** de perder el punto de cordura (como en la solución alternativa).
- **TP:** como `canDefeat` depende de la **paridad de la longitud del propósito**, los propósitos traducidos al inglés se eligieron con la misma paridad (par) que los originales. Hay un test que lo verifica: traducir datos puede cambiar la semántica.

---

## 2. Refactored Code (English Haskell)

El código completo vive en `refactored/src/`. Acá van los fragmentos que mejor muestran cada decisión, con su "antes y después".

### 2.1 Un único punto de cambio + aplicación parcial (TP)

**Antes** (`Library.hs`): cada alimento suma o resta a mano, con secciones encadenadas difíciles de leer.

```haskell
modificarResistencia :: Persona -> Number -> Persona
modificarResistencia persona valor = persona{nivelDeResistencia = resistenciaPersona + valor}
    where resistenciaPersona = nivelDeResistencia persona

beberSustanciaX persona = modificarResistencia persona resistenciaNegativa
    where resistenciaNegativa = (*(-1)).nivelDeResistencia $ persona
```

**Después** (`PowerpuffGirls.hs`): el modificador recibe una **función** (orden superior), y cada alimento es una aplicación parcial de una sola línea.

```haskell
modifyResistance :: (Int -> Int) -> Person -> Person
modifyResistance change person = person {resistance = change (resistance person)}

substanceX :: Food
substanceX = modifyResistance (const 0)

liquidCaramel :: Food
liquidCaramel = modifyResistance (subtract 10)

flavouring :: String -> Food
flavouring flavour = modifyResistance (subtract (length flavour))
```

### 2.2 Lista de predicados en vez de `&&` encadenados (TP)

**Antes:**

```haskell
esNivelAlto amenaza = (((tieneCantidadParDeDebilidades amenaza &&).(> 50).danioPotencialDe $ amenaza) &&).noEsDebilAKriptonita $ amenaza
```

**Después:** las condiciones son **datos** (una lista de funciones) y `all ($ threat)` las evalúa.

```haskell
isHighLevel :: Threat -> Bool
isHighLevel threat = all ($ threat) highLevelConditions
  where
    highLevelConditions :: [Threat -> Bool]
    highLevelConditions =
      [ even . length . weaknesses
      , notElem kryptonite . weaknesses
      , (> 50) . potentialDamage
      ]
```

### 2.3 Guardas sólo donde deciden algo (TP)

**Antes:** el `where` decide por la mitad entera o por la división real (`Number` permite `/`).

```haskell
puedeVencerAmenaza amenaza persona
    | even.length.proposito $ amenaza = resistenciaMayorA.(/2).danioPotencialDe $ amenaza
    | otherwise = resistenciaMayorA.danioPotencialDe $ amenaza
```

**Después:** `r > d / 2` se reescribe como `2 * r > d`, que es equivalente en los enteros y evita `Fractional`.

```haskell
canDefeat :: Threat -> Person -> Bool
canDefeat threat person
  | even (length (purpose threat)) = 2 * resistance person > damage
  | otherwise = resistance person > damage
  where
    damage = potentialDamage threat
```

En `attack`, la guarda elige **qué función** aplicar (no qué valor devolver):

```haskell
attack :: Threat -> CityEffect
attack threat city = extraEffect . rumourOfAttack threat $ city
  where
    extraEffect
      | canAttack threat city = effect threat
      | otherwise = id
```

### 2.4 Folds en vez de recursión explícita (TP, Samurai Jack, Huber v2)

```haskell
-- TP: la maratón es un foldl sobre los capítulos.
marathon :: City -> Season -> City
marathon = foldl play

-- Samurai Jack: aplicar n veces una función es componer n copias.
applyTimes :: Int -> (a -> a) -> a -> a
applyTimes n f = foldr (.) id (replicate n f)
```

**Antes** (Huber v2), recursión manual con patrón incompleto:

```haskell
choferConMenosViajes [chofer] = chofer
choferConMenosViajes (chofer1:chofer2:choferes) = choferConMenosViajes ((elQueMenosViajesHizo chofer1 chofer2):choferes)
```

**Después:** función de orden superior de la biblioteca + `Maybe` para el caso vacío.

```haskell
leastBusy :: [Driver] -> Maybe Driver
leastBusy [] = Nothing
leastBusy drivers = Just (minimumBy (comparing tripCount) drivers)

assignTrip :: Trip -> [Driver] -> Maybe Driver
assignTrip trip = fmap (addTrip trip) . leastBusy . filter (canTake trip)
```

### 2.5 Funciones totales con `Maybe` (Huber v1)

**Antes:**

```haskell
estanOrdenadas u1 u2 lista = indexOf u1 lista < indexOf u2 lista
indexOf e lista = recIndexOf 1 e lista
recIndexOf _ _ [] = 0 -- es feo :)
```

**Después:** un único helper genérico (`Eq a => ...`) del que se derivan las demás funciones.

```haskell
stopsBetween :: Eq a => a -> a -> [a] -> Maybe Int
stopsBetween start end xs =
  case (elemIndex start xs, elemIndex end xs) of
    (Just i, Just j) | i <= j -> Just (j - i)
    _ -> Nothing

areOrdered :: Eq a => a -> a -> [a] -> Bool
areOrdered start end = maybe False (> 0) . stopsBetween start end

busFare :: Location -> Location -> BusLine -> Maybe Int
busFare start end = fmap (2 *) . mfilter (> 0) . stopsBetween start end . stops

distanceBetween :: Location -> Location -> [Location] -> Maybe Int
distanceBetween start end = fmap (4 *) . stopsBetween start end
```

### 2.6 Un combinador genérico en lugar de dos copias (FedEx, CurryCoin)

**Antes** (FedEx): `aplicarCargo` (suma un monto) y `aplicarCargo'` (aplica una función) son casi iguales.

**Después:** un único `chargeWhen`; los cargos concretos son aplicaciones parciales.

```haskell
chargeWhen :: (Shipment -> Bool) -> (Shipment -> Price -> Price) -> Charge
chargeWhen condition priceChange shipment
  | condition shipment = shipment {basePrice = priceChange shipment (basePrice shipment)}
  | otherwise = shipment

categoryCharge :: Category -> Double -> Charge
categoryCharge category percentage =
  chargeWhen (hasCategory category) (const (* (1 + percentage / 100)))

overweightCharge :: Double -> Charge
overweightCharge limitKg =
  chargeWhen ((> limitKg) . weightKg) (\shipment -> (+ 80 * (weightKg shipment - limitKg)))
```

**Antes** (CurryCoin): `head`, `takeWhile`, `dropWhile` y `tail` para "modificar la primera cuenta que cumple".

```haskell
sinLaPrimeraQueCumple condicion cuentas = (takeWhile (/= primera) cuentas ++).tail.(dropWhile (/= primera)) $ cuentas
    where primera = primeraQueCumple condicion cuentas
```

**Después:** `break` parte la lista una sola vez; el helper es polimórfico y conserva el orden.

```haskell
updateFirst :: (a -> Bool) -> (a -> a) -> [a] -> [a]
updateFirst condition change xs =
  case break condition xs of
    (before, x : after) -> before ++ change x : after
    (before, []) -> before

modifyAccount :: AccountId -> Transaction -> [Account] -> [Account]
modifyAccount wanted = updateFirst (hasId wanted)
```

### 2.7 ADTs con nombre en vez de tuplas y strings (Pisteando, Samurai Jack, Huber v2)

```haskell
-- Antes: desgaste :: (Number, Number) --(Ruedas, Chasis)
data Wear = Wear {wheels :: Double, chassis :: Double}

-- Antes: tipo :: String, comparado contra "Maldad"
data ElementKind = Magic | Evil

-- Antes: fecha :: (Number, Number, Number)
data Date = Date {day :: Int, month :: Int, year :: Int}
```

### 2.8 Funciones que decoran funciones (Pisteando)

Boxes, mojado, ripio y obstrucción **envuelven** otro tramo: son de tipo `Section -> Section`.

```haskell
pitStop :: Section -> Section
pitStop section car
  | isInGoodShape car = section car
  | otherwise = section . addTime 10 . repair $ car

wet :: Section -> Section
wet section car = addTime (timeTaken / 2) afterSection
  where
    afterSection = section car
    timeTaken = raceTime afterSection - raceTime car

gravel :: Section -> Section
gravel section = section . section

obstructed :: Double -> Section -> Section
obstructed metres section = addWheelWear (2 * metres) . section
```

### 2.9 Evaluación perezosa como herramienta de diseño (Pisteando, Lovecraft, Samurai Jack, TP)

```haskell
-- Pisteando: lista infinita de vueltas; sólo se calculan las que pide la carrera.
playRace :: Race -> [Car] -> [[Car]]
playRace race = take (laps race) . drop 1 . iterate nextLap
  where
    nextLap = filter (not . isWrecked) . completeLap (track race)

-- TP: un valor que se refiere a sí mismo.
mojoJojo = Threat { ..., effect = rumourOfAttack mojoJojo }

-- Samurai Jack: el portal de Aku crea otro Aku (definición recursiva).
futurePortalFrom year = Element Magic (sendToYear futureYear) (aku futureYear . health)
```

### 2.10 Pizzerías como funciones componibles (Pizza Conmigo)

```haskell
type Pizzeria = Order -> Order

gourmetPizzeria :: Int -> Pizzeria
gourmetPizzeria exquisiteness = map enlarge . filter ((> exquisiteness) . satisfaction)

jaujaPizzeria :: Pizzeria
jaujaPizzeria = gourmetPizzeria 399

favouritePizzeria :: [Pizzeria] -> Pizzeria
favouritePizzeria = foldr (.) id
```

---

## 3. Engineering Justification Matrix

KAs de SWEBOK v4.0 citadas: **Software Design (SD)**, **Software Construction (SC)**, **Software Testing (ST)**, **Software Quality (SQ)** y **Software Maintenance (SM)**. Características de ISO/IEC 25010: Mantenibilidad (modularidad, reusabilidad, analizabilidad, modificabilidad, testeabilidad), Eficiencia de desempeño (comportamiento temporal, uso de recursos) y Adecuación funcional (corrección).

| # | Change Implemented | SWEBOK v4.0 Alignment | ISO/IEC 25010 Impact | Deep-Dive Explanation |
|---|---|---|---|---|
| 1 | Prelude estándar en lugar de `PdePreludat`; `Number` reemplazado por `Int`/`Double`/`Float` según el dominio | SC (uso de estándares y lenguajes), SD (elección de tipos) | Portabilidad (adaptabilidad); Adecuación funcional (corrección): `Int` hace exactas las cuentas de habitantes, saldos y porciones | `Number` es un tipo "todo en uno" que oculta si una cuenta es entera o real. Al elegir el tipo, el compilador **prohíbe** `div` sobre reales o `/` sobre enteros. Por eso `r > d / 2` se reescribió como `2 * r > d`: misma relación en ℤ (multiplicar ambos lados por 2 > 0 preserva el orden) sin salir de los enteros. |
| 2 | Firmas de tipo explícitas en todo, `-Wall` en cero | SC (convenciones de codificación), SQ (análisis estático) | Analizabilidad, testeabilidad | Haskell **infiere** los tipos (Hindley–Milner), así que la firma no es necesaria para compilar. Se escribe igual porque es documentación verificada por el compilador: si la implementación infiere un tipo distinto, falla la compilación. Ejemplo didáctico: el parcial pedía inferir `myPrecious`; GHC infiere `(Foldable t, Num n) => [(a, b -> n)] -> ((a, b -> n) -> t c, b) -> n`, más general que la respuesta escrita en el intento (que fijaba listas y `Number`). |
| 3 | Funciones totales: `Maybe` en búsquedas, `drop 1` en vez de `tail`, `foldr (.) id` en vez de `foldl1 (.)`, `maximum . (0 :)`, `applyTimes` en vez de `!!` | SD (manejo de errores y excepciones), SQ (defectos) | Fiabilidad (madurez, tolerancia a fallos); Adecuación funcional (corrección) | Una función parcial **miente en su tipo**: `[a] -> a` promete un `a` incluso para `[]`. `[a] -> Maybe a` hace visible el caso vacío y obliga a quien llama a tratarlo (con `maybe`, `fmap`, pattern matching). `foldr (.) id` usa que `id` es el **elemento neutro** de la composición (el monoide de los endomorfismos), así que la lista vacía tiene un resultado razonable: la identidad. |
| 4 | Combinadores de orden superior (`modifyResistance`, `modifySanity`, `modifyHealth`, `chargeWhen`, `updateFirst`) | SD (abstracción, ocultamiento de información), SC (reutilización) | Reusabilidad, modificabilidad | "Qué campo cambia" y "cómo cambia" se separan: el combinador sabe actualizar el registro; la función que recibe sabe la regla. Agregar un alimento nuevo es escribir una función `Int -> Int`, sin tocar la estructura de `Person`. Es el principio "una sola razón para cambiar" aplicado a funciones. |
| 5 | Composición y aplicación parcial (estilo point-free donde se lee bien) | SD (diseño por composición), SC (legibilidad) | Analizabilidad, modularidad | `groupHasItem = any . hasItemNamed`, `deadlyEnemies = filter . isDeadlyEnemy`, `scariestEvent = maximumOn . terror`. Matemáticamente `(f . g) x = f (g x)`; componer construye funciones nuevas **sin nombrar el dato**. Se evitó point-free cuando obliga a usar `flip` o `(.) . (.)`, porque ahí la legibilidad empeora. |
| 6 | Folds en vez de recursión explícita (`marathon = foldl play`, `applyBlock`, `useElements`, `sufferConsequences`) | SD (patrones de diseño), SC | Analizabilidad, modificabilidad | `foldl f z [x1, x2, x3] = f (f (f z x1) x2) x3`: el fold **es** la recursión estructural sobre listas, escrita una sola vez en la biblioteca. Usarlo hace explícito el orden de aplicación (izquierda a derecha), que era la causa del bug #4 (`foldr ($)` aplicaba los alimentos al revés). |
| 7 | Lista de predicados + `all ($ x)` (`isHighLevel`) | SD (datos dirigiendo comportamiento) | Modificabilidad, analizabilidad | Las funciones son **valores de primera clase**: se pueden guardar en listas. Agregar una condición es agregar un elemento a la lista, no reescribir una cadena de `&&`. `($ x)` es la sección que "aplica a x". |
| 8 | ADTs con nombre (`Wear`, `Date`, `BusLine`, `ElementKind`, `Location`) en vez de tuplas y strings | SD (modelado del dominio, tipos de datos), SQ (prevención de defectos) | Analizabilidad, Adecuación funcional (corrección) | Un tipo suma como `data ElementKind = Magic \| Evil` tiene exactamente dos habitantes: un typo como `"Maldad "` pasa a ser un error de compilación. Un tipo producto con campos (`Wear {wheels, chassis}`) evita confundir `fst` con `snd`. Los tipos son **proposiciones** que el compilador demuestra. |
| 9 | Dependencias explícitas (catálogos de Huber por parámetro) | SD (acoplamiento y cohesión), ST (diseño para la testeabilidad) | Testeabilidad, modularidad | Una función que lee una constante global sigue siendo pura, pero queda **acoplada** a esos datos. Recibirlos por parámetro permite testear con otros catálogos y reutilizar la lógica. Gracias a la currificación, `huberFare rides` sigue siendo una función de dos ubicaciones. |
| 10 | Decoradores `Section -> Section` (Pisteando) | SD (patrón Decorator, composición) | Reusabilidad, modificabilidad | En OO un decorador envuelve un objeto; en funcional es una **función que recibe y devuelve una función**. `gravel section = section . section` es literalmente "dos veces la función". Así `obstructed 2 (curve 80 400)` combina comportamientos sin crear tipos nuevos. |
| 11 | `iterate` + `take` para las vueltas; listas infinitas en Nito y en el 4.c de Lovecraft | SD (estrategias de evaluación), SQ | Eficiencia de desempeño (comportamiento temporal, uso de recursos) | **Evaluación perezosa:** una expresión se evalúa sólo cuando se necesita su valor y a lo sumo una vez. `iterate nextLap cars` describe infinitas vueltas, pero `take 20` sólo fuerza 20. `canTake trip nito` termina aunque `trips nito` sea infinita porque la condición no mira la lista. `head (filter isSane (cycle group))` termina en cuanto aparece el primer cuerdo. Lo que **no** termina es lo que necesita toda la lista: `sum`, `length`, `minimumBy (comparing tripCount)` sobre Nito. |
| 12 | Transparencia referencial aprovechada en tests y refactors | SQ (verificación), ST (pruebas basadas en propiedades) | Testeabilidad, analizabilidad | Como ninguna función tiene efectos, una expresión puede reemplazarse por su valor sin cambiar el programa. Eso habilita **razonamiento ecuacional**: `gravel shortStraight ferrari == (shortStraight . shortStraight) ferrari` es un test válido; `marathon c (xs ++ ys) == marathon (marathon c xs) ys` es una ley de `foldl` que QuickCheck verifica con datos aleatorios. |
| 13 | `Show` manual para tipos con funciones (`Threat`, `Shipment`, `Driver`, `Event`, `Element`) | SC (depuración), ST | Analizabilidad | Las funciones no tienen `Show`. En vez de `Text.Show.Functions` (que imprime `<function>` y oculta todo), se muestran los campos que sí son datos. El de `Driver` muestra sólo los primeros 3 viajes: así `show nito` termina pese a la lista infinita. |
| 14 | Consolidación de soluciones duplicadas (Lovecraft, Pizza) y separación de Huber en dos módulos | SM (refactorización, comprensión del programa), SD (modularidad) | Modularidad, analizabilidad | Dos soluciones del mismo enunciado se fusionaron tomando la parte correcta de cada una (tabla 1.4). Las dos de Huber resolvían **enunciados distintos** (colectivos/Huber vs. choferes/viajes), así que van en módulos separados (`Exams.Huber.BusRoutes`, `Exams.Huber.Drivers`). |
| 15 | Export lists explícitas en todos los módulos | SD (interfaces, ocultamiento de información) | Modularidad | La lista de exportación es la interfaz pública del módulo, agrupada por punto del enunciado. Helpers como `kryptonite` quedan privados. |

### 3.1 Apunte para estudiar: los tres conceptos clave

**Transparencia referencial.** Una expresión es referencialmente transparente si puede reemplazarse por su valor en cualquier contexto sin cambiar el resultado del programa. En Haskell todas las funciones son puras, así que `potentialDamage mojoJojo` vale `64` siempre. Consecuencias prácticas en este refactor: (a) los tests no necesitan preparar ni limpiar estado; (b) se puede reordenar o extraer código con seguridad (por ejemplo, el `where damage = potentialDamage threat` de `canDefeat` es equivalente a escribir la llamada dos veces); (c) las propiedades QuickCheck son ecuaciones entre expresiones.

**Evaluación perezosa (lazy evaluation).** Haskell evalúa *por necesidad* (call-by-need): los argumentos se pasan como *thunks* (cálculos pendientes) y se evalúan la primera vez que se usan. Por eso:
- `mojoJojo` puede referirse a sí mismo dentro de su propio registro;
- `aku` contiene un portal que construye otro `aku`, sin recursión infinita al definirlo;
- `take (laps race) . iterate nextLap` es eficiente: el costo es proporcional a las vueltas pedidas;
- `firstMemberPotentialDelta (insaneLuis : cycle [pepe, luis])` termina (hay un test que lo prueba), lo que **refuta** la respuesta teórica original del punto 4.c de Lovecraft para el caso b.

Contracara: `foldl` perezoso acumula thunks; en listas grandes conviene `foldl'` (de `Data.List`). Acá las listas son chicas y se priorizó la cercanía con lo que se ve en la materia.

**Tipado estático inferido.** El compilador deduce el tipo más general de cada expresión (unificación de Hindley–Milner) y lo verifica **antes** de ejecutar. Ejemplo del parcial de Samurai Jack:

```haskell
inferenceExercise x y z
  | y 0 == z = map (fst . x z)
  | otherwise = map (snd . x (y 0))
-- y 0      ⇒ y :: t2 -> t1 con Num t2
-- y 0 == z ⇒ z :: t1 con Eq t1
-- x z      ⇒ x :: t1 -> a1 -> (a2, a2)  (fst y snd deben dar el mismo tipo,
--                                          porque las dos ramas devuelven [a2])
inferenceExercise :: (Eq t1, Num t2) => (t1 -> a1 -> (a2, a2)) -> (t2 -> t1) -> t1 -> [a1] -> [a2]
```

Escribir la firma igual sirve para fijar la intención y obtener mejores mensajes de error.

---

## 4. Verification & Clean Code Recommendations

### 4.1 Cómo correr todo

Desde `refactored/`:

```bash
stack build            # compila con -Wall (debe dar 0 warnings)
stack test             # corre HSpec + QuickCheck (hspec-discover encuentra todos los *Spec.hs)
stack test --ta "--match /PowerpuffGirls/"   # sólo el TP
stack test --ta "--match /Exams.Lovecraft/"  # sólo un parcial
stack ghci             # REPL con todos los módulos cargados
```

Resultado al cerrar este refactor: **442 ejemplos, 0 fallas** en todo el paquete; de ellos, **194 corresponden al TP y los parciales**:

| Módulo de tests | Ejemplos |
|---|---|
| `PowerpuffGirlsSpec` | 57 |
| `Exams.CurryCoinSpec` | 14 |
| `Exams.FedExSpec` | 10 |
| `Exams.Huber.BusRoutesSpec` | 15 |
| `Exams.Huber.DriversSpec` | 14 |
| `Exams.LovecraftSpec` | 23 |
| `Exams.PizzaConmigoSpec` | 24 |
| `Exams.RacingSpec` | 22 |
| `Exams.SamuraiJackSpec` | 15 |

### 4.2 Qué se testea

- **HSpec (ejemplos):** todos los casos de prueba del enunciado del TP portados al inglés (con los bugs del Spec original corregidos), más el caso faltante de la cocucha. En los parciales, un test por punto resuelto y un **test de regresión por cada bug** de la tabla 1.4.
- **QuickCheck (propiedades):** generadores explícitos con `forAll` (sin instancias `Arbitrary` huérfanas). Ejemplos de leyes verificadas:
  - idempotencia: `beer xs . beer xs == beer xs`, `addIngredient i . addIngredient i == addIngredient i`;
  - invariantes: la población y la cordura nunca son negativas; `enlarge` nunca supera 10 porciones; ningún tramo hace retroceder el tiempo;
  - leyes de fold y composición: `marathon c (xs ++ ys) == marathon (marathon c xs) ys`, `favouritePizzeria (xs ++ ys) == favouritePizzeria xs . favouritePizzeria ys`, `applyTimes n f . applyTimes m f == applyTimes (n + m) f`;
  - conservación: un bloque de pago + cobro del mismo monto conserva el saldo total.
- **Pereza:** tests que sólo terminan si la evaluación es perezosa (Nito, `gongNeng` sobre `[1 ..]`, grupo infinito de Lovecraft, `show nito`).
- Los valores `Double` se comparan con tolerancia (`closeTo`), nunca con `==` sobre resultados de divisiones.

### 4.3 Próximos pasos recomendados

1. **HLint.** No está instalado en este entorno. Instalarlo con `stack install hlint` y correr `hlint src test` desde `refactored/`. Sugerencias típicas a evaluar caso por caso (no aplicar a ciegas si empeoran la legibilidad): eta-reducción, `concatMap`, uso de `notElem`.
2. **Formateo automático** con `ormolu` o `fourmolu` (el código ya sigue su estilo: comas al inicio, 2 espacios) para que el diff entre versiones sea sólo semántico.
3. **Cobertura:** `stack test --coverage` genera un reporte HPC; usarlo para encontrar ramas de guardas sin test.
4. **`foldl'` estricto** si alguna vez se procesan listas grandes (por ejemplo, una blockchain larga en CurryCoin), para no acumular thunks.
5. **`NonEmpty`** (`Data.List.NonEmpty`) como alternativa a `Maybe` cuando el dominio garantiza al menos un elemento (por ejemplo, un grupo de investigadores): la totalidad se obtiene en el tipo de entrada en vez de en el de salida.
6. **Enunciados:** conseguir los enunciados de los parciales y validar los supuestos de la sección 1.5, sobre todo las fórmulas de Pisteando.
7. **CI:** el TP original tenía workflows de GitHub Actions y CircleCI; replicar uno que corra `stack build --pedantic` y `stack test` sobre `refactored/`.

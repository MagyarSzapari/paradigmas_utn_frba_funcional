# Refactor de las guías de ejercicios (v2.0)

Alcance: el mini proyecto `guias-ejercicios/src/Library.hs` (≈1000 líneas,
basado en `PdePreludat`) y su `Spec.hs`. El código nuevo vive en
`refactored/src/Guides/` y sus pruebas en `refactored/test/Guides/`.
El original queda intacto como referencia.

> Convención del documento: la prosa está en español; el código, los
> identificadores y los comentarios del código están en inglés.

---

## 1. Executive Summary & Functional Diagnostic

### 1.1 Diagnóstico del código original

El `Library.hs` original es un cuaderno de cursada: un único módulo con
todas las guías (1 a 4), cinco TPs/prácticas (Casino Royale, Pizza Conmigo,
Huber, CurryCoin) y dos parciales de práctica (Lovecraft, Samurai Jack).
Funciona como apunte, pero como software tiene estos problemas:

| Problema | Ejemplo en el original | Consecuencia |
|---|---|---|
| Módulo monolítico | 1021 líneas, 11 temas en `module Library` | Baja modularidad; choques de nombres (`dispersion` aparece dos veces, `maximoSegun` definido y luego comentado) |
| Tipo único `Number` | Todo es `Number` (de `PdePreludat`) | Se pierde información: un índice, una nota y una temperatura tienen el mismo tipo; `!! (m/10)` compila aunque conceptualmente es una división real |
| Funciones parciales | `head`, `tail`, `!!`, `foldl1`, `maximum`, guardas no exhaustivas | Errores en tiempo de ejecución con listas vacías o casos borde |
| Valores centinela | `-1` como "no rindió el recuperatorio"; `0` como "no está en la lista" | El tipo no distingue el caso "ausente": provocó un bug real en Huber |
| Strings mágicos | `"Magia"`, `"Maldad"` | Un typo es un `False` silencioso en vez de un error de compilación |
| Recursión explícita donde hay un combinador | `aplicarFunciones`, `subirHabilidad` | Más código, más superficie para errores |
| Firmas faltantes | `ocurrenciasDe`, `palo`, `jamesBond`, `indexOf`, `estaMuerto`... | `-Wall` las marca; el lector no ve el contrato |
| Ejercicios incompletos o comentados | Guía 1 ej. 12, Guía 3 ej. 6–7, Guía 4 ej. 9 y 21, llamadas 4.a/4.b | Huecos en la práctica |
| Comparaciones con booleanos | `(diasParejos x y z) == False` | Ruido; `not` expresa lo mismo |

Hay también aciertos que se conservaron: uso intensivo de secciones
(`(+1)`, `(1/)`), composición (`esPesoUtil . pesoPino`), `type` alias para
funciones (`type Transaccion = Cuenta -> Cuenta`, `type Pizzeria = Pedido -> Pedido`)
y abstracciones como `modificarSalud` en Samurai Jack.

### 1.2 Bugs encontrados en el original

Todos están cubiertos por un test que documenta el comportamiento correcto
(los nombres de los tests mencionan "the original ...").

| # | Original | Bug | Corrección |
|---|---|---|---|
| 1 | `esBisiesto` (G1 ej. 5) | `mult 400 \|\| (mult 4 && mult 100)`: exige ser múltiplo de 100, así que 2024 "no es bisiesto" | `mult 400 \|\| (mult 4 && not (mult 100))`. Curiosamente `esBisiesto2` y `esBisiestoAP` eran correctas |
| 2 | `cuentaBizarra` (G3 ej. 3) | Última guarda `y-x < 10 && y-x > 0`: `(5,5)` y `(0,10)` no matchean ninguna guarda y explotan | Última guarda `otherwise` |
| 3 | `mcm` (G1 ej. 9) | `x*y / gcd x y` con `gcd 0 0 = 0` divide por cero | Se usa `lcm` del Prelude |
| 4 | `nuevoIngrediente` (Pizza 4.a) | Calcula `2 + calorias * letras` | `calorias + 2 * letras` (el doble de las letras) |
| 5 | `calculoCalorias` (Pizza 4.c) | Calcula `calorias1 / (2 + calorias2)` | `calorias2 + calorias1 / 2` (la segunda más la mitad de la primera) |
| 6 | `estanOrdenadas` (Huber) | `indexOf` devuelve `0` si no encuentra; como `0 < k`, si falta la primera ubicación responde `True`, contradiciendo su propio comentario | `elemIndex` devuelve `Maybe Int`; ausencia ⇒ `False` |
| 7 | `cantidadDeParadasEntre` (Huber) | `tail []` explota si el origen no está en el recorrido | `drop 1` (total) |
| 8 | `blockchainEstable'` (CurryCoin 6) | Cada bloque se aplica a las cuentas **iniciales**; el efecto de los bloques anteriores se pierde | `scanl` acumula los estados intermedios |
| 9 | `modificarCuenta` (CurryCoin 3) | Mueve la cuenta modificada al principio de la lista y explota (`head []`) si el id no existe | Conserva la posición; id inexistente ⇒ lista sin cambios |
| 10 | `primerIntegrante` (Lovecraft 4.b) | `filter ((0 ==) . cordura)` se queda **sólo con los locos** (potencial 0 por definición), así que el resultado siempre es el potencial del primero | `filter ((> 0) . sanity)`: se quitan los locos (interpretación del enunciado, ver nota) |
| 11 | `potencial` (Lovecraft 3) | `maximoSegun valor (items ...)` explota si el investigador no tiene ítems | `maximum (0 : map value items)` |
| 12 | `mayorSegun'` (Casino 1.a) | Devuelve `f x1` en dos ramas y `x1` en la tercera: mezcla "elemento" con "medida" | Devuelve siempre el elemento |
| 13 | `maximoSegun'` (Casino 1.b) | Devuelve el máximo de las imágenes, no el elemento que maximiza | Devuelve el elemento (`Maybe`) |
| 14 | `cuandoHabloMasMinutos` (G4) | Usaba un valor (`duracionLlamadas`) como si fuera un tipo; por eso estaba comentado | Se modeló `CallLog` y se implementó |

> Nota sobre el bug 10: el enunciado habla de "filtrar a los locos". La
> versión original es degenerada (siempre mide contra 0), por eso se
> interpretó como "quitar a los locos". Si la cátedra lo pedía literal, el
> cambio es invertir el predicado.

Otros detalles menores: `concatenar = foldl (++) []` es cuadrático (ver
§3, cambio C9); `aproboG4 = (>=6) . minimum` explota con `[]`;
`laPizzeriaPredilecta = foldl1 (.)` explota con `[]`; `promedio` de una
lista vacía da `NaN`; `esMultiploDeAP` tenía los argumentos al revés
respecto de `esMultiploDe` pese a llamarse igual.

### 1.3 Nivel de abstracción alcanzado

- **11 módulos temáticos + 1 de utilidades**, cada uno con lista de
  exportación explícita.
- **Cero warnings** con `-Wall -Wcompat -Wincomplete-uni-patterns
  -Wincomplete-record-updates -Wredundant-constraints`.
- **Funciones totales** en todo el código nuevo: donde el original podía
  explotar, ahora hay `Maybe`, un valor neutro documentado o un patrón
  exhaustivo. Las únicas recursiones posiblemente infinitas (`fight`) son
  inherentes al enunciado y están documentadas.
- **ADTs** en lugar de tuplas anónimas y strings mágicos: `CallLog`,
  `BusLine`, `Ride`, `Location`, `Suit`, `ElementKind`, `Retakes`
  (`Maybe Grade`).
- **248 ejemplos HSpec** (incluye 30+ propiedades QuickCheck), todos en verde.

---

## 2. Refactored Code (English Haskell)

### 2.1 Mapa de módulos

| Módulo | Contenido del original | Ideas centrales |
|---|---|---|
| `Guides.Utils` | `maximoSegun`, `deltaSegun`, promedio | Combinadores reutilizables, totalidad |
| `Guides.Basics` | Guía 1 | Guardas, composición, lista infinita perezosa (`isPerfectSquare`) |
| `Guides.PartialApplication` | Guía 2 | Secciones, currificación, point-free |
| `Guides.Tuples` | Guía 3 | Pattern matching sobre tuplas, `Maybe` en vez de `-1` |
| `Guides.Lists` | Guía 4 (Listas) | `take`/`drop` totales, record `CallLog` |
| `Guides.HigherOrder` | Guía 4 (Orden Superior y OS + Listas, folds) | `map`, `filter`, `any`, `all`, `takeWhile`, `foldl'` |
| `Guides.Casino` | TP Casino Royale | ADT `Suit` + `Bounded`/`Enum`, `foldr` vs `foldl` |
| `Guides.Pizza` | Pizza Conmigo | Records, funciones como datos (`Pizzeria`), `foldr (.) id` |
| `Guides.Huber` | Huber | ADT `Location`, `Maybe` + `Applicative`, `find` |
| `Guides.CurryCoin` | CurryCoin | Transacciones como funciones, `break`, `scanl` |
| `Guides.Lovecraft` | Lovecraft | `deltaBy`, `maximumOn`, totalidad |
| `Guides.SamuraiJack` | Samurai Jack | Instancias manuales de `Show`/`Eq`, `iterate`, datos recursivos perezosos |

Pruebas: un `test/Guides/<Módulo>Spec.hs` por módulo, cada uno exporta
`spec :: Spec` y es descubierto automáticamente por `hspec-discover`.

### 2.2 Tabla de equivalencias (original → refactor)

Para encontrar cada ejercicio rápido. Las funciones marcadas con ★ estaban
incompletas o comentadas en el original y se completaron.

**Guía 1 → `Guides.Basics`**

| Original | Refactor |
|---|---|
| `doble` | `double` |
| `esMultiploDeTres` | `isMultipleOfThree` |
| `esMultiploDe` | `isMultipleOf` |
| `cubo` | `cube` |
| `area` | `triangleArea` |
| `esBisiesto` / `esBisiesto2` | `isLeapYear` / `isLeapYearGuards` |
| `celsiusToFahr` / `fahrToCelsius` | `celsiusToFahrenheit` / `fahrenheitToCelsius` |
| `haceFrioF` | `isColdFahrenheit` |
| `mcm` | `leastCommonMultiple` |
| `dispersion` | `dispersion` |
| `diasParejos` / `diasLocos` / `diasNormales` | `areEvenDays` / `areCrazyDays` / `areNormalDays` |
| `pesoPino` / `esPesoUtil` / `sirvePino` | `pineWeight` / `isUsefulWeight` / `isUsefulPine` |
| ★ `esCuadradoPerfecto` | `isPerfectSquare` |

**Guía 2 → `Guides.PartialApplication`**

| Original | Refactor |
|---|---|
| `siguiente`, `mitad`, `inversa`, `triple` | `successor`, `half`, `inverse`, `triple` |
| `esNumeroPositivo` | `isPositive` |
| `esMultiploDeAP` | `divides` (los argumentos estaban al revés; el nombre nuevo lo aclara) |
| `esBisiestoAP` | `isLeapYearPointFree` |
| `inversaRaizCuadrada` | `inverseSquareRoot` |
| `incrementMCuadradoN` | `addSquareOf` |
| `esResultadoPar` | `isPowerEven` |

**Guía 3 → `Guides.Tuples`**

| Original | Refactor |
|---|---|
| `fst3`, `snd3`, `trd3` | `fst3`, `snd3`, `thd3` |
| `aplicar` | `applyBoth` |
| `cuentaBizarra` | `oddCalculation` |
| `esNotaBochazo` | `isFailingGrade` |
| `aproboG3` | `passedBoth` |
| `promociono` | `promoted` |
| `aproboPP` | `passedFirstPartial` |
| `notasFinales` | `finalGrades` |
| `recursa` | `mustRetakeCourse` |
| `recuperoPP` | `retookFirstPartial` |
| `recuperoDeGusto` (+ `recuperoConProm`) | `retookForFun` |
| ★ `esMayorDeEdad` | `isOfAge` |
| ★ `calcular` | `calculate` |

**Guía 4 (Listas) → `Guides.Lists`**

| Original | Refactor |
|---|---|
| `sumaDeListas` | `sumList` |
| `frecuenciaCardiaca` | `heartRates` |
| `promedioFrecuenciaCardiaca` | `averageHeartRate` |
| `frecuenciaCardiacaMinuto` | `heartRateAt` (ahora `Maybe Int`) |
| `frecuenciasHastaMomento` | `heartRatesUntil` |
| `esCapicua` | `isPalindrome` |
| `duracionLlamadas` | `callLogs` (con el record `CallLog`) |
| ★ `cuandoHabloMasMinutos` / ★ `cuandoHizoMasLlamadas` | `bandWithMoreMinutes` / `bandWithMoreCalls` |

**Guía 4 (Orden Superior, OS + Listas) → `Guides.HigherOrder`**

| Original | Refactor |
|---|---|
| `existsAny` | `existsInTriple` |
| `mejor` | `better` |
| `aplicarPar` | `applyToPair` |
| `parDeFns` | `pairOfFunctions` |
| `esMultiploDeAlguno` | `isMultipleOfAny` |
| `prom` / `promedios` | `Guides.Utils.average` / `averages` |
| `promediosSinAplazos` | `averagesWithoutFails` |
| `mejoresNotas` | `bestGrades` |
| `aproboG4` / `aprobaron` | `passed` / `passedStudents` |
| `divisores` | `divisors` |
| `exists` | `exists` |
| ★ `hayAlgunNegativo` | `hasAnyNegative` |
| `aplicarFunciones` | `applyFunctions` |
| `sumaF` | `sumOfFunctions` |
| `subirHabilidad` | `raiseSkills` |
| `flimitada` | `clamped` |
| `cambiarHabilidad` | `changeSkills` |
| (respuesta 13.b) | `raiseLowSkillsToFour` |
| `primerosPares` / `primerosDivisores` / `primerosNoDivisores` | `leadingEvens` / `leadingDivisorsOf` / `leadingNonDivisorsOf` |
| `sumatoriaLista` / `productoriaLista` | `sumWithFold` / `productWithFold` |
| ★ ej. 21 (`dispersion` con fold) | `listDispersion` |

**Casino Royale → `Guides.Casino`**

| Original | Refactor |
|---|---|
| `palo` | `Suit` (ADT) + `suitName` + `validSuitNames` |
| `Carta` (tupla) | `Card` (record) |
| `Jugador` | `Player` |
| `pokerDeAses`, `fullDeJokers`, `piernaDeNueves` | `fourAces`, `jokersFullHouse`, `ninesLeg` |
| `jamesBond`, `leChiffre`, `felixLeiter`, `mesaQueMasAplauda` | `jamesBond`, `leChiffre`, `felixLeiter`, `table` |
| `ocurrenciasDe`, `concatenar` | `occurrencesOf`, `concatenate` |
| `mayorSegun'` | `largerBy` |
| `maximoSegun'` / `maximoSegunCompYAP` | `maximumBy'` |
| `sinRepetidos` | `withoutDuplicates` |
| `esoNoSeVale` | `isInvalidCard` |

**Pizza Conmigo → `Guides.Pizza`**

| Original | Refactor |
|---|---|
| `Pizza {ingredientes, tamanio, calorias}` | `Pizza {ingredients, slices, calories}` |
| `grandeDeMuzza`, `pizzaDeAnchoasBasica` | `largeMozzarella`, `basicAnchovy` |
| `nivelDeSatisfaccion` | `satisfaction` |
| `valorDePizza` | `price` |
| `nuevoIngrediente` | `addIngredient` |
| `agrandar` | `enlarge` |
| `mezcladita` (+ `mezclaDeIngredientes`, `calculoCalorias`, `agregarSinRepetir`) | `mix` (usa `union`) |
| `nivelDeSatisfaccionPedido` | `orderSatisfaction` |
| `Pedido`, `Pizzeria` | `Order`, `Pizzeria` |
| `pizzeriaLosHijosDePato` | `losHijosDePato` |
| `pizzeriaElResumen` / `pizzeriaElResumen'` | `elResumen` |
| `pizzeriaEspecial` / `pizzeriaPescadito` | `specialPizzeria` / `pescadito` |
| `pizzeriaGourmet` / `pizzeriaLaJauja` | `gourmet` / `laJauja` |
| `sonDignasDeCalleCorrientes` | `worthyOfCorrientes` |
| `mejorPizzeria` (+ `maximoSegun`) | `bestPizzeria` (+ `Guides.Utils.maximumOn`) |
| `laPizzeriaPredilecta` | `favouritePizzeria` |

**Huber → `Guides.Huber`**

| Original | Refactor |
|---|---|
| `Ubicacion = Interseccion \| Altura` | `Location = Intersection \| Address` |
| `Persona`, `huberto`, `leto` | `Person`, `huberto`, `leto` |
| `colectivos :: [(Number, [Ubicacion])]` | `busLines :: [BusLine]` |
| `Huber`, `hubers` | `Ride`, `rides` |
| `ubicaciones` (+ `ubicacion1..3`) | `locations` |
| `estanOrdenadas` (+ `indexOf`, `recIndexOf`) | `areInOrder` (usa `elemIndex`) |
| `importeEnHuber` / `importeEnHuber'` | `huberFare` |
| `sePuedeIrEnBondi` / `realizaViajeEnBondi` | `canGoByBus` / `servesTrip` (interna) |
| `masBaratasQueHuber` | `cheaperLinesThanHuber` |
| `importeEnColectivo` | `busFare` |
| `cantidadDeParadasEntre` (+ `dropWhileEnd` propio) | `stopsBetween` (usa `Data.List.dropWhileEnd`) |
| `distanciaEntre` | `distanceBetween` |

**CurryCoin → `Guides.CurryCoin`**

| Original | Refactor |
|---|---|
| `Cuenta {idCuenta, saldo}`, `Id` | `Account {accountId, balance}`, `AccountId` |
| `Transaccion`, `Bloque`, `Blockchain` | `Transaction`, `Block`, `Blockchain` |
| `transaccion`, `pago`, `cobro`, `mineria` | `transaction`, `payment`, `collection`, `mining` |
| `correspondeId` | `hasId` |
| `primeraQueCumple` | `firstMatching` (`Maybe`) |
| `sinLaPrimeraQueCumple` | `withoutFirstMatching` (polimórfica) |
| `modificarCuenta` | `updateAccount` |
| `ejecutarTransacciones` | `runBlock` |
| `sonEstables` | `areStable` |
| `blockchainEstable'` | `isStableBlockchain` |
| `funcionSinPudor` | `shamelessFunction` |

**Lovecraft → `Guides.Lovecraft`**

| Original | Refactor |
|---|---|
| `Investigador {nombreInv, cordura, items, sucesosEvitados}` | `Investigator {investigatorName, sanity, items, avoidedEvents}` |
| `Item {nombreItem, valor}` | `Item {itemName, value}` |
| `CambioEstado`, `CambioEstadoGrupal`, `Grupo` | `StateChange`, `GroupChange`, `Group` |
| `pepe`, `luis`, `necronomicon`, `dagaMaldita` | `pepe`, `luis`, `necronomicon`, `cursedDagger` |
| `deltaSegun` | `Guides.Utils.deltaBy` |
| `enloquecer` (+ `locura`) | `goMad` |
| `hallarItem` | `findItem` |
| `tieneItem` | `groupHasItem` |
| `lider` | `leader` (`Maybe`) |
| `potencial`, `experiencia` | `potential`, `experience` |
| `corduraTotal` | `totalSanityDelta` |
| `primerIntegrante` | `firstSaneMemberDelta` |

**Samurai Jack → `Guides.SamuraiJack`**

| Original | Refactor |
|---|---|
| `Personaje = UnPersonaje {...}` | `Character {characterName, health, elements, currentYear}` |
| `Elemento = UnElemento {tipo, ataque, defensa}` | `Element {kind, attack, defense}` con `kind :: ElementKind` |
| `"Magia"`, `"Maldad"` | `Magic`, `Evil` |
| `mandarAlAnio` / `mandarAlAnio'` | `sendToYear` |
| `meditar`, `causarDanio`, `modificarSalud` | `meditate`, `takeDamage`, `modifyHealth` |
| `esMalvado`, `esDeTipo` | `isEvil`, `isOfKind` |
| `danioQueProduce` / `danioQueProduce'` | `damageDealt` |
| `enemigosMortales`, `esEnemigoMortal` | `deadlyEnemies`, `isDeadlyEnemy` |
| `tieneAtaqueMortal` / `tieneAtaqueMortal'` | `hasDeadlyAttack` |
| `estaMuerto`, `noHacerNada` | `isDead`, `doNothing` |
| `concentracion`, `esbirrosMalvados`, `unEsbirro`, `katanaMagica` | `concentration`, `evilMinions`, `minion`, `magicKatana` |
| `jack`, `aku`, `portalAlFuturoDesde` | `jack`, `aku`, `futurePortalFrom` |
| `luchar`, `usarElementos`, `afectar` | `fight`, `useElements` (`afectar` = `flip ($)`) |
| `f` (punto 5, inferencia) | `inferenceExample` |

### 2.3 Fragmentos clave (antes / después)

El código completo está en `refactored/src/Guides/`. Acá van los cambios
más ilustrativos.

#### a) Recursión explícita → `map` (Guía 4, ej. 10 y 12)

Antes:

```haskell
aplicarFunciones :: [Number -> a] -> Number -> [a]
aplicarFunciones [] _ = []
aplicarFunciones (f:fs) x = (f x : aplicarFunciones fs x)

subirHabilidad :: Number -> [Number] -> [Number]
subirHabilidad _ [] = []
subirHabilidad x (f:fs)
    | ((f + x) <= 12) = (f + x : subirHabilidad x fs)
    | otherwise = 12 : subirHabilidad x fs
```

Después:

```haskell
applyFunctions :: [a -> b] -> a -> [b]
applyFunctions fs x = map ($ x) fs

raiseSkills :: Int -> [Int] -> [Int]
raiseSkills amount = map (min maxSkill . (+ amount))
```

Las dos funciones originales eran `map` "escrito a mano". `($ x)` es una
sección del operador de aplicación: "la función que recibe una función y
la aplica a `x`". La guarda `if f + x <= 12 then f + x else 12` es
exactamente `min 12 (f + x)`, así que se expresa por composición. Además
`applyFunctions` dejó de estar atada a `Number`: el tipo inferido es
totalmente polimórfico.

#### b) Centinela `-1` → `Maybe` (Guía 3, ej. 5)

Antes:

```haskell
notasFinales :: ((Number, Number), (Number, Number)) -> (Number, Number)
notasFinales ((x, y), (z, t)) = (max x z, max y t)

recuperoPP ((_, _), (z, _)) = not (z == -1)
```

Después:

```haskell
type Retakes = (Maybe Grade, Maybe Grade)
type StudentRecord = (Grades, Retakes)

finalGrades :: StudentRecord -> Grades
finalGrades ((first, second), (retake1, retake2)) =
  (bestOf first retake1, bestOf second retake2)
  where
    bestOf grade = maybe grade (max grade)

retookFirstPartial :: StudentRecord -> Bool
retookFirstPartial (_, (retake1, _)) = isJust retake1
```

Con `-1` el tipo `Number` mentía: "no rendí" y "me saqué -1" eran el mismo
valor. `Maybe Grade` hace el caso ausente **explícito en el tipo**, y el
compilador obliga a tratarlo. `maybe grade (max grade)` se lee: "si no hay
recuperatorio, la nota; si lo hay, el máximo".

#### c) Índice `0` como "no encontrado" → `elemIndex` + `Applicative` (Huber)

Antes:

```haskell
estanOrdenadas u1 u2 lista = indexOf u1 lista < indexOf u2 lista

indexOf e lista = recIndexOf 1 e lista
recIndexOf _ _ [] = 0 -- es feo :)
recIndexOf index e (x:xs)
    | e==x = index
    | otherwise = recIndexOf (index+1) e xs
```

Después:

```haskell
areInOrder :: Location -> Location -> [Location] -> Bool
areInOrder from to xs =
  ((<) <$> elemIndex from xs <*> elemIndex to xs) == Just True
```

El comentario original ("es feo") tenía razón y además escondía un bug: si
`u1` falta, su índice es `0` y `0 < k` da `True`. `elemIndex` devuelve
`Maybe Int`; `(<) <$> mi <*> mj` aplica la comparación sólo si **ambos**
existen (si alguno es `Nothing`, el resultado es `Nothing`, que no es
`Just True`).

#### d) Estados intermedios con `scanl` (CurryCoin, punto 6)

Antes:

```haskell
blockchainEstable' :: Blockchain -> [Cuenta] -> Bool
blockchainEstable' [] _ = True
blockchainEstable' (x:xs) cuentas
    | sonEstables (ejecutarTransacciones x cuentas) = blockchainEstable' xs cuentas
    | otherwise = False
```

Después:

```haskell
isStableBlockchain :: Blockchain -> [Account] -> Bool
isStableBlockchain chain accounts =
  all areStable (drop 1 (scanl (flip runBlock) accounts chain))
```

En la recursión original, la llamada recursiva vuelve a pasar `cuentas`
(las iniciales), no las cuentas después del bloque: cada bloque se evalúa
aislado. `scanl` es un `foldl` que devuelve **todos** los acumuladores
intermedios: `scanl f z [b1, b2] = [z, f z b1, f (f z b1) b2]`. Así el
código dice literalmente "todos los estados intermedios son estables".

#### e) ADT en vez de strings mágicos (Samurai Jack)

Antes:

```haskell
data Elemento = UnElemento { tipo :: String, ... }
esMalvado personaje = (any (esDeTipo "Maldad") . elementos) personaje
```

Después:

```haskell
data ElementKind = Magic | Evil
  deriving (Show, Eq)

isEvil :: Character -> Bool
isEvil = any (isOfKind Evil) . elements
```

Escribir `"Maldd"` compilaba y devolvía `False` siempre. Escribir `Evl`
es un error de compilación.

#### f) Pattern matching en vez de `length` (Pizza, 6.b)

Antes:

```haskell
pizzeriaElResumen pedido
    | length pedido == 1 = pedido
    | otherwise = zipWith mezcladita pedido $ tail pedido
```

Después:

```haskell
elResumen :: Pizzeria
elResumen order@(_ : rest@(_ : _)) = zipWith mix order rest
elResumen order = order
```

`length` recorre toda la lista sólo para saber si tiene un elemento; el
patrón `(_ : rest@(_ : _))` pregunta "¿tiene al menos dos?" mirando sólo
los dos primeros nodos. El *as-pattern* `rest@` evita llamar a `tail`.

#### g) Totalidad con `foldr (.) id` (Pizza, 9)

```haskell
-- Before: laPizzeriaPredilecta = foldl1 (.)     -- crashes on []
favouritePizzeria :: [Pizzeria] -> Pizzeria
favouritePizzeria = foldr (.) id
```

`id` es el elemento neutro de la composición (`f . id = f = id . f`), así
como `0` lo es de la suma. Darle al fold su neutro lo vuelve total: la
composición de cero pizzerías es "no hacer nada".

---

## 3. Engineering Justification Matrix

KAs de SWEBOK v4.0 usadas: *Software Design*, *Software Construction*,
*Software Testing*, *Software Maintenance*, *Software Quality*.
Características de ISO/IEC 25010: *Maintainability* (modularity,
reusability, analysability, modifiability, testability), *Performance
efficiency* (time behaviour, resource utilization), *Functional
suitability* (functional correctness) y *Reliability* (faultlessness /
fault tolerance).

| # | Change Implemented | SWEBOK v4.0 Alignment | ISO/IEC 25010 Impact | Deep-Dive Explanation |
|---|---|---|---|---|
| C1 | Partir el monolito en 12 módulos con exportaciones explícitas | *Software Design* (principios de diseño: modularización, cohesión, acoplamiento, ocultamiento de información) | Maintainability → modularity, analysability | Cada módulo es una unidad cohesiva (un tema/TP). La lista de exportación es la interfaz pública: `servesTrip` en Huber queda oculta. Los choques de nombres (dos `dispersion`, dos `maximoSegun`) desaparecen porque cada nombre vive en su espacio. |
| C2 | Firmas de tipo explícitas en todo y `Number` → `Int`/`Double` según el dominio | *Software Construction* (construcción para verificación, estándares de codificación); *Software Design* (especificación de interfaces) | Maintainability → analysability; Functional suitability → correctness | La firma es una especificación chequeada por el compilador. Distinguir `Int` (conteos, índices, notas) de `Double` (temperaturas, calorías) convierte errores de dominio en errores de tipo: `!! (m / 10)` ya no compila; hay que escribir `` m `div` 10 ``. |
| C3 | Recursión explícita → `map`/`filter`/`any`/`all`/`takeWhile` | *Software Construction* (reutilización, minimizar complejidad); *Software Design* (abstracción) | Maintainability → reusability, modifiability | Los combinadores capturan esquemas de recursión ya probados. `map f` cumple las leyes `map id = id` y `map (f . g) = map f . map g`; razonar sobre `map ($ x) fs` es más fácil que sobre una recursión a mano. Menos casos base escritos = menos lugares donde equivocarse. |
| C4 | Funciones parciales → totales (`Maybe`, neutros, patrones exhaustivos) | *Software Quality* (prevención de defectos); *Software Construction* (programación defensiva) | Reliability → fault tolerance; Functional suitability → correctness | Una función total está definida para todo su dominio. `head`, `tail`, `!!`, `foldl1`, `maximum` no lo son. Con `Maybe a` el "puede fallar" queda en el tipo y el llamador decide qué hacer. Cuando hay un neutro natural (`0` para potencial sin ítems, `id` para la composición) se usa ese. |
| C5 | Centinelas (`-1`, índice `0`) → `Maybe` | *Software Design* (diseño de estructuras de datos); *Software Quality* (análisis de defectos) | Functional suitability → correctness; Maintainability → analysability | El centinela es un valor válido del tipo usado con otro significado: el compilador no puede distinguirlo. Fue la causa directa del bug 6. `Maybe` es un ADT con dos constructores: `Nothing` (ausente) y `Just x` (presente). |
| C6 | Tuplas anónimas y strings → ADTs (`CallLog`, `BusLine`, `Ride`, `Location`, `Suit`, `ElementKind`) | *Software Design* (modelado del dominio, tipos de datos abstractos) | Maintainability → analysability, modifiability | Un ADT es una suma (`\|`) de productos (campos). `Location = Intersection Street Street \| Address Street Int` modela "o una esquina o una altura" sin estados ilegales. Los records dan nombres: `lineNumber line` en vez de `fst colectivo`. "Hacer irrepresentables los estados ilegales". |
| C7 | Abstraer lógica repetida (`bandWithMoreBy`, `maximumOn`, `deltaBy`, `modifyHealth`, `goMad` reutilizado por `findItem`) | *Software Design* (principio DRY, abstracción); *Software Maintenance* (reducción de deuda técnica) | Maintainability → reusability, modifiability | Las dos funciones de llamadas difieren sólo en la medida (`sum` vs `length`): se parametriza la medida como función (orden superior). `findItem` reutiliza `goMad` en vez de duplicar `max 0 (...)`: si cambia la regla de cordura, cambia en un lugar. |
| C8 | Point-free y composición donde es legible (`isColdFahrenheit = (< 8) . fahrenheitToCelsius`, `isEvil = any (isOfKind Evil) . elements`) | *Software Construction* (estilo y legibilidad) | Maintainability → analysability | La composición `(f . g) x = f (g x)` describe un *pipeline* de transformaciones. Se evitó el point-free forzado: `isLeapYearPointFree` se conserva como ejercicio, pero la versión "de producción" (`isLeapYear`) es explícita porque se lee mejor. |
| C9 | `foldl (++) []` → `foldr (++) []`; `foldl` → `foldl'` en sumas; `foldl'` + `seq` en `listDispersion` | *Software Construction* (consideraciones de performance); *Software Design* (selección de algoritmos) | Performance efficiency → time behaviour, resource utilization | `(++)` cuesta O(n) en su argumento **izquierdo**. Con `foldl`, el acumulador (cada vez más largo) queda a la izquierda: costo total O(n²). Con `foldr`, cada lista se copia una sola vez: O(n), y además funciona con listas infinitas. `foldl'` evalúa el acumulador en cada paso y evita una cadena de *thunks* `((0+1)+2)+...` que consume memoria. |
| C10 | `any` + `head . filter` → `find`; `takeWhile` + `dropWhile` → `break` | *Software Construction* (eficiencia, uso de bibliotecas estándar) | Performance efficiency → time behaviour; Maintainability → analysability | `importeEnHuber` recorría la lista dos veces (una para `any`, otra para `filter`). `find` recorre una sola vez y devuelve `Maybe`. `break p xs` parte la lista en un único recorrido. |
| C11 | `scanl` para estados intermedios; `iterate` + `(!!)`; `takeWhile` sobre `[0 ..]` | *Software Design* (estrategias de diseño funcional) | Performance efficiency → time behaviour; Functional suitability → correctness | Evaluación perezosa: estas listas son potencialmente infinitas o largas, pero sólo se calcula lo que se consume. `all` corta en el primer `False`, así que `isStableBlockchain` no ejecuta los bloques posteriores a uno inestable. |
| C12 | Corrección de los 14 bugs con un test de regresión cada uno | *Software Quality* (V&V, análisis de defectos); *Software Testing* (pruebas de regresión) | Functional suitability → correctness | Cada bug quedó "fijado" por un test cuyo nombre explica qué hacía el original. Si alguien reintroduce el error, el test falla. |
| C13 | Suite HSpec + QuickCheck por módulo | *Software Testing* (niveles de prueba, pruebas basadas en propiedades); *Software Quality* | Maintainability → testability | Los tests de ejemplo fijan casos concretos; las propiedades QuickCheck fijan **leyes** que deben valer para cualquier entrada (100 casos aleatorios cada una). Ver §4. |
| C14 | `Bounded`/`Enum` derivados para `Suit` (`validSuitNames = map suitName [minBound .. maxBound]`) | *Software Design* (una única fuente de verdad) | Maintainability → modifiability | Agregar un palo al ADT actualiza automáticamente la lista de palos válidos: no hay una lista paralela que mantener a mano. |
| C15 | Instancias manuales `Show`/`Eq` para `Element` | *Software Design* (diseño de interfaces/typeclasses) | Maintainability → testability | Los records con funciones no pueden derivar `Eq` (la igualdad de funciones es indecidible). Definirlas por `kind` permite comparar personajes en los tests con `shouldBe`. |

### 3.1 Conceptos teóricos para estudiar

#### Evaluación perezosa (lazy evaluation)

Haskell evalúa una expresión **recién cuando su valor se necesita** y
**como mucho una vez** (*call-by-need*). Ejemplos del refactor:

- `isPerfectSquare n = n `elem` takeWhile (<= n) (map (^ 2) [0 ..])`:
  `[0 ..]` es infinita. `takeWhile` deja de pedir elementos cuando un
  cuadrado supera `n`, y `elem` deja de mirar cuando encuentra `n`. Sólo se
  calculan ≈√n cuadrados.
- `concentration level = ... defense = (!! level) . iterate meditate`:
  `iterate meditate c = [c, meditate c, meditate (meditate c), ...]` es
  infinita; `(!! 3)` fuerza sólo los primeros cuatro nodos.
- `aku`: su portal tiene como defensa `aku future . health`, que a su vez
  tiene un portal... una estructura conceptualmente infinita que no explota
  porque nada se construye hasta que alguien usa el portal.
- `elResumen` original: `zipWith f [] (tail [])` **no explota** aunque
  `tail []` sea un error, porque `zipWith` ve que la primera lista es vacía
  y nunca evalúa la segunda. Es un ejemplo de que el original "andaba de
  casualidad" gracias a la pereza; el refactor no depende de eso.
- `concatenate (repeat [1, 2])` con `foldr` devuelve sus primeros
  elementos; con `foldl` se colgaría para siempre (hay un test).
- Ejercicio 4.c de Lovecraft (respuesta teórica): con un grupo infinito,
  `totalSanityDelta` no termina (`sum` necesita todos los elementos), y
  `firstSaneMemberDelta` **sí** puede terminar si hay algún cuerdo, porque
  `listToMaybe . filter p` sólo necesita el primer elemento que cumpla `p`.
  Si nadie está cuerdo, `filter` busca para siempre.

Contracara: la pereza puede acumular *thunks* (cálculos pendientes). Por
eso las sumas usan `foldl'` (estricto en el acumulador) y `listDispersion`
fuerza ambas componentes del par con `seq`: `foldl'` sólo evalúa a *forma
normal débil de cabeza* (WHNF), que para un par es el constructor `(,)`,
no su contenido.

#### Transparencia referencial

Una expresión es referencialmente transparente si puede reemplazarse por su
valor sin cambiar el significado del programa. Todas las funciones del
refactor son **puras**: mismo argumento ⇒ mismo resultado, sin efectos.
Consecuencias prácticas:

- **Razonamiento ecuacional**: podemos probar que `raiseSkills` nunca supera
  12 "reemplazando iguales por iguales":
  `map (min 12 . (+ n)) xs` ⇒ cada elemento es `min 12 (x + n)` ⇒ `≤ 12`.
  Exactamente eso verifica la propiedad QuickCheck correspondiente.
- **Refactor seguro**: en `isPalindrome` se nombró `text = concat wordsList`
  en un `where`. Por transparencia referencial, usar `text` dos veces es
  idéntico a escribir `concat wordsList` dos veces, pero se calcula una.
- **Las "modificaciones" crean valores nuevos**: `addIngredient` no muta la
  pizza; devuelve otra. Por eso el test de idempotencia
  `addIngredient i (addIngredient i p) == addIngredient i p` tiene sentido:
  `p` sigue siendo el mismo valor durante toda la prueba.
- **Funciones como datos**: `type Transaction = Account -> Account` y
  `type Pizzeria = Order -> Order` sólo son razonables porque las funciones
  son puras; una transacción guardada en un bloque hace siempre lo mismo.

#### Tipado estático con inferencia

GHC deduce el tipo más general de cada expresión (algoritmo de
Hindley–Milner). Las firmas explícitas no son obligatorias para compilar,
pero sí documentan y **restringen** la intención.

- Al quitar `Number`, varias firmas se generalizaron solas:
  `applyBoth :: (a -> b, a -> c) -> a -> (b, c)` (antes atada a `Number`),
  `withoutFirstMatching :: (a -> Bool) -> [a] -> [a]`,
  `largerBy :: Ord b => (a -> b) -> a -> a -> a`.
- `inferenceExample` (punto 5 de Samurai Jack) es el ejercicio de
  inferencia de la cátedra. El razonamiento:
  - `y 0` ⇒ `y` es una función cuyo argumento es un literal numérico: `Num n => n -> b`.
  - `y 0 == z` ⇒ `z :: b` y `Eq b`.
  - `x z` ⇒ `x` recibe un `b`; `fst . x z` y `snd . x (y 0)` ⇒ `x z` devuelve un par cuyos dos componentes tienen el mismo tipo (ambas ramas devuelven lo mismo): `x :: b -> a -> (c, c)`.
  - `map (...)` sin lista explícita ⇒ el resultado es `[a] -> [c]`.

  Resultado: `(Eq b, Num n) => (b -> a -> (c, c)) -> (n -> b) -> b -> [a] -> [c]`.
  El original tenía un typo (`Numb t2`); el refactor compila con la firma
  escrita, lo que prueba que es correcta.
- `shamelessFunction` (CurryCoin 7) es otro ejercicio de inferencia: `even`
  impone `Integral n` sobre la imagen de `f`.
- `-Wall` en los tests advierte del *defaulting* (cuando GHC elige `Integer`
  a falta de información). Por eso los tests anotan literales como
  `(3 :: Int)`: es un recordatorio de que los literales numéricos en
  Haskell son polimórficos (`3 :: Num a => a`).

---

## 4. Verification & Clean Code Recommendations

### 4.1 Cómo correr las pruebas

```bash
cd refactored
stack test                                # toda la suite (TP, parciales y guías)
stack test --ta '--match "Guides"'        # sólo las guías
stack ghci                                # REPL con todos los módulos
```

Resultado de esta proceso para las guías: **248 ejemplos, 0 fallos**,
compilando con `-Wall` y las demás banderas del `package.yaml` sin un solo
warning.

### 4.2 Qué cubren las pruebas

- **Un `describe` por ejercicio** con casos del enunciado, casos borde
  (listas vacías, empates, valores límite como 300 cm en el pino o 10
  porciones) y un test de regresión por cada bug de §1.2.
- **Propiedades QuickCheck** (leyes que valen para toda entrada), por ejemplo:
  - `isLeapYear == isLeapYearGuards == isLeapYearPointFree` (tres
    implementaciones, una especificación).
  - `fahrenheitToCelsius . celsiusToFahrenheit ≈ id` (tolerancia relativa,
    porque `Double` no es exacto).
  - `dispersion` es invariante por permutaciones y nunca negativa; exactamente
    una de "parejos / locos / normales" vale siempre.
  - `withoutDuplicates == nub`, `concatenate == concat`,
    `sumWithFold == sum`, `productWithFold == product`,
    `listDispersion xs == maximum xs - minimum xs` para listas no vacías.
  - `raiseSkills` nunca supera 12; `clamped f x ∈ [0, 12]`.
  - `addIngredient` es idempotente; `enlarge` nunca supera 10 porciones
    (con un generador propio de pizzas, `genPizza`).
  - `payment a . collection a == id` (inversas).
  - `withoutFirstMatching` quita a lo sumo un elemento.
  - `maximumOn id == Just . maximum` en listas no vacías; `deltaBy` ≥ 0.

### 4.3 Recomendaciones

1. **HLint**: correr `hlint src/Guides test/Guides`. Se escribió el código
   siguiendo sus sugerencias habituales (eta-reducción, `isJust` en vez de
   `/= Nothing`, `any` en vez de `or . map`), pero no está instalado en este
   entorno; conviene agregarlo al flujo (`stack install hlint`).
2. **Formateo automático** con `ormolu` o `fourmolu` para un estilo uniforme.
3. **Cobertura**: `stack test --coverage` genera un reporte HPC que muestra
   qué ramas de guardas no se ejercitan.
4. **`NonEmpty`**: donde hoy se devuelve `Maybe` por la lista vacía
   (`maximumOn`, `leader`, `bestPizzeria`), una alternativa más fuerte es
   recibir `Data.List.NonEmpty.NonEmpty a`: el caso vacío deja de existir
   en vez de manejarse.
5. **`newtype` para unidades**: `newtype Celsius = Celsius Double` y
   `newtype Fahrenheit = Fahrenheit Double` harían imposible pasar una
   temperatura en la escala equivocada, sin costo en tiempo de ejecución.
6. **Smart constructors**: en Casino, `Card` mantiene el palo como `String`
   a propósito (el ejercicio consiste en detectar cartas inválidas como
   `"Copas"`). En un sistema real convendría `parseCard :: (Int, String) -> Maybe Card`
   con `suit :: Suit`, validando una vez en el borde del sistema.
7. **Ejercicios no implementados**: Guía 4 ej. 16–18, Casino 2.b (`manoNegra`)
   y 3–7, y Huber 4.b y siguientes estaban vacíos en el original y sin
   enunciado en el repositorio, así que no se inventaron. Guía 3 ej. 6–7 y
   Guía 4 ej. 21 se completaron según el enunciado de la guía; si la
   consigna difiere, ajustar la función y su test.

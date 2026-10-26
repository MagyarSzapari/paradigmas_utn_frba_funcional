module Library where
import PdePreludat

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
type Blockchain = [Bloque]

ejecutarTransacciones :: Bloque -> [Cuenta] -> [Cuenta]
ejecutarTransacciones bloque cuentas = foldl aplicarTransax cuentas bloque
    where
        aplicarTransax :: [Cuenta] -> (Id, Transaccion) -> [Cuenta]
        aplicarTransax cuentas (identificador, transax) = modificarCuenta identificador cuentas transax















--4
afectar bloque cuentas = foldl aplicarTrx cuentas bloque
    where aplicarTrx cs (identificador, trx) = modificarCuenta identificador cs trx

--5-------------------------------------------------------------------



--6-------------------------------------------------------------------
type Blockchain = [Bloque]


--7-------------------------------------------------------------------
--funcionSinPudor:: [[c]] -> ( (c -> Number), ( a -> a ) ) -> ( a -> a )

# Paradigmas de Programación — Paradigma Funcional (Haskell)

Código de la cursada de **Paradigmas de Programación** (UTN FRBA) correspondiente al paradigma funcional.

## Estructura

```
paradigmas-funcional-haskell/
├── tp-chicas-superpoderosas/   TP grupal 2023 (proyecto Stack + PdePreludat, tests con Hspec)
│   ├── src/  test/             Código y specs tal como se entregaron
│   ├── historial/              Versión de la primera entrega
│   └── docs/                   Enunciado y código entregado (PDF)
├── guias-ejercicios/           Mini proyecto Stack con los ejercicios de las guías
└── parciales/                  Parciales de práctica (un directorio por enunciado)
    ├── currycoin/  fedex/  huber/  lovecraft/  pisteando-como-un-campeon/
    ├── pizza-conmigo/  samurai-jack/
    └── recuperatorio-peleadores/   (solo enunciado)
```

## Cómo correrlo

Los proyectos de la cursada usan [PdePreludat](https://github.com/10Pines/pdepreludat) (resolver `lts-18.14`):

```bash
cd tp-chicas-superpoderosas
stack test
```

Los parciales son módulos sueltos que importan `PdePreludat`; se pueden pegar en `src/Library.hs` de cualquiera de los proyectos para probarlos con `stack ghci`.

## Refactor (v2.0)

`refactored/` es un proyecto Stack independiente (GHC 9.6, Prelude estándar, código en inglés) con el TP, las guías y los parciales reescritos, y 442 tests Hspec/QuickCheck:

```bash
cd refactored
stack test
```

La justificación de cada cambio (SWEBOK v4.0, ISO/IEC 25010) y los bugs encontrados en los originales están en [`docs/REFACTOR.md`](docs/REFACTOR.md) (TP y parciales) y [`docs/REFACTOR-guias.md`](docs/REFACTOR-guias.md) (guías).

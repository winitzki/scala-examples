let Prelude = https://prelude.dhall-lang.org/v20.2.0/package.dhall

let Arith = ./naturals.dhall

-- This division function returns 0 when trying to divide by 0.
let div: Natural -> Natural -> Natural = \(x: Natural) -> \(y: Natural) -> Prelude.Optional.default Natural 0 (Arith.div x y)

let Unit: Type = < Unit >

let unit = Unit.Unit : Unit

-- There can be no values of type Void.
let Void = forall (x: Type) -> x

-- Having a value of type Void is enough to produce a value of any type.
let absurd: forall (t: Type) -> Void -> t = \(t: Type) -> \(v: Void) -> v t

let Nonzero: Natural -> Type = \(y: Natural) -> if Natural/isZero y then Void else Unit

let test = unit : Nonzero 1

-- division where we require the divisor to be nonzero
let divide: Natural -> forall (y: Natural) -> Nonzero y -> Natural =
  \(x: Natural) -> \(y: Natural) -> \(_ : Nonzero y) ->
   div x y

let test = assert : divide 40 20 unit === 2
-- let test = assert : divide 40 0 unit === 2   -- This fails to typecheck!

let nonzeroToNatural: forall (x: Natural) -> Nonzero x -> Natural = \(x: Natural) -> \(_ : Nonzero x) -> x

{-  This does not type-check because Dhall cannot see that y == 0 after "then".
let xdiv: forall (y: Natural) -> Nonzero y -> Natural = \(y: Natural) -> \(x: Nonzero y) ->
   if Natural/isZero y then absurd Natural x else div x y

let test = assert : xdiv 20 40 === 2
-- let test = assert : xdiv 0 40 === 2
-}
in {divide}

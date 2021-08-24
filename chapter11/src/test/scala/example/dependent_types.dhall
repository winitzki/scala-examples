let Prelude = https://prelude.dhall-lang.org/v20.2.0/package.dhall

let Arith = ./naturals.dhall

let Void = forall (x: Type) -> x -- there can be no values of type Void

let NonzeroTypeWrapper = < Zero: Void | Nonzero: Natural >

let WrapNonzero: Natural -> NonzeroTypeWrapper = \(x: Natural) -> if Prelude.Natural.isZero x then NonzeroTypeWrapper.Zero Void else NonzeroTypeWrapper.Nonzero Natural

-- does not work

let isNonzero: forall (y: Natural) -> WrapNonzero (y + 1) = \(y: Natural) ->
    let z: NonzeroTypeWrapper.Nonzero (y + 1) = y
    in
    z

-- division where we require the divisor to be nonzero
let divide: Natural -> (y: Natural) -> WrapNonzero y -> Natural =
  \(x: Natural) -> \(y: Natural) -> \(nonzero: WrapNonzero y) ->
    Prelude.Optional.default Natural 0 (Arith.div x y)

let test = assert : divide 40 20 (isNonzero 19) === 2


in {divide}

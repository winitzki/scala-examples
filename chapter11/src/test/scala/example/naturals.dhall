-- Implement some arithmetical operations for Natural numbers.

-- Division with remainder, integer binary logarithm, integer square root, greatest common divisor, primality test.

let Prelude = https://prelude.dhall-lang.org/v20.2.0/package.dhall

-- (safeSubtract x y) returns None when subtraction x - y would have given a negative number.
let safeSubtract: Natural -> Natural -> Optional Natural = \(x: Natural) -> \(y: Natural) ->
  if Prelude.Natural.greaterThanEqual x y then Some (Natural/subtract y x) else None Natural

let test = assert : safeSubtract 10 5 === Some 5
let test = assert : safeSubtract 5 10 === None Natural

-- (downwardProgression a b) computes the list of the form [ a - b, a - 2*b, ... ] until the subtraction would
-- have given a negative number.
let downwardProgression = \(x: Natural) -> \(y: Natural) ->
  Prelude.List.unpackOptionals Natural (Prelude.List.map Natural (Optional Natural) (\(n: Natural) -> safeSubtract  x (n * y)) (Prelude.List.drop 1 Natural (Prelude.Natural.enumerate (x + 1))))

let test = assert : downwardProgression 15 4 === [11, 7, 3]
let test = assert : downwardProgression 5 2 === [3, 1]
let test = assert : downwardProgression 4 2 === [2, 0]
let test = assert : downwardProgression 4 1 === [3, 2, 1, 0]
let test = assert : downwardProgression 4 5 === ([ ] : List Natural)

let xsafeDivide: Natural -> Natural -> Optional Natural = \(x: Natural) -> \(y: Natural) ->
  let result = Prelude.List.length Natural (downwardProgression x y)
  in
  if Prelude.Natural.isZero y then None Natural else Some result

let downwardSteps = \(x: Natural) -> \(y: Natural) ->
  let Acc: Type = { remainder: Natural, length: Natural, done: Bool }
  let emptyAcc: Acc = { remainder = x, length = 0, done = False }
  let updateAcc: Acc -> Acc = \(acc: Acc) -> if acc.done then acc else
    if Prelude.Natural.lessThan acc.remainder y then acc // { done = True }
    else acc // {remainder = Natural/subtract y acc.remainder, length = acc.length + 1}
  in
  if Prelude.Natural.isZero y then emptyAcc
  else if Prelude.Natural.isZero x then emptyAcc // { done = True }
  else Natural/fold (x + 1) Acc updateAcc emptyAcc

let test = assert : downwardSteps 15 4 === {remainder = 3, length = 3, done = True }
let test = assert : downwardSteps 5 2 === {remainder = 1, length = 2, done = True }
let test = assert : downwardSteps 4 2 === {remainder = 0, length = 2, done = True }
let test = assert : downwardSteps 4 1 === {remainder = 0, length = 4, done = True }
let test = assert : downwardSteps 4 5 === {remainder = 4, length = 0, done = True }
let test = assert : downwardSteps 4 0 === {remainder = 4, length = 0, done = False }


let safeDivide: Natural -> Natural -> Optional Natural = \(x: Natural) -> \(y: Natural) ->
    let result = downwardSteps x y
    in
    if result.done then Some (result.length) else None Natural

let test = assert : safeDivide 5 2 === Some 2
let test = assert : safeDivide 5 3 === Some 1
let test = assert : safeDivide 1 2 === Some 0
let test = assert : safeDivide 0 2 === Some 0
let test = assert : safeDivide 4 5 === Some 0
let test = assert : safeDivide 4 2 === Some 2
let test = assert : safeDivide 3 3 === Some 1
let test = assert : safeDivide 3 0 === None Natural
let test = assert : safeDivide 0 0 === None Natural
let test = assert : safeDivide 15 4 === Some 3
let test = assert : safeDivide 4 1 === Some 4

let xsafeRemainder: Natural -> Natural -> Optional Natural = \(x: Natural) -> \(y: Natural) ->
  let result = Prelude.List.last Natural (downwardProgression x y)
  in
  if Prelude.Natural.isZero y then None Natural else if Prelude.Natural.lessThan x y then Some x else result

let safeRemainder: Natural -> Natural -> Optional Natural = \(x: Natural) -> \(y: Natural) ->
    let result = downwardSteps x y
    in
    if result.done then Some (result.remainder) else None Natural

let test = assert : safeRemainder 15 4 === Some 3
let test = assert : safeRemainder 5 2 === Some 1
let test = assert : safeRemainder 5 3 === Some 2
let test = assert : safeRemainder 0 2 === Some 0
let test = assert : safeRemainder 4 5 === Some 4
let test = assert : safeRemainder 4 2 === Some 0
let test = assert : safeRemainder 4 1 === Some 0
let test = assert : safeRemainder 3 3 === Some 0
let test = assert : safeRemainder 3 0 === None Natural
let test = assert : safeRemainder 0 0 === None Natural


-- (binaryLog n) is how many times we need to divide n by 2 to obtain 0.
let binaryLog: Natural -> Optional Natural = \(x: Natural) ->
  let Acc: Type = {current: Natural, count: Natural, done: Bool }
  let updateAcc: Acc -> Acc = \(acc: Acc) -> if acc.done then acc else if Prelude.Natural.isZero acc.current then acc // { done = True } else
    let divided = safeDivide acc.current 2
    in
    merge { Some = \(p: Natural) -> acc // {current = p, count = acc.count + 1}, None = acc // {count = 999} } divided
  let emptyAcc: Acc = {current = x, count = 0, done = False }
    let test = assert : updateAcc { current = 0, count = 1, done = False } === { current = 0, count = 1, done = True }
    let test = assert : updateAcc { current = 10, count = 2, done = False } === { current = 5, count = 3, done = False }
    let test = assert : updateAcc { current = 1, count = 0, done = False } === { current = 0, count = 1, done = False }
    let test = assert : Natural/fold 2 Acc updateAcc {current = 1, count = 0, done = False } === { current = 0, count = 1, done = True }
  let result = Natural/fold (x + 1) Acc updateAcc emptyAcc
  in
  if Prelude.Natural.isZero x then None Natural else Some result.count

let test = assert : binaryLog 0 === None Natural
let test = assert : binaryLog 1 === Some 1
let test = assert : binaryLog 2 === Some 2
let test = assert : binaryLog 3 === Some 2
let test = assert : binaryLog 4 === Some 3
let test = assert : binaryLog 5 === Some 3
let test = assert : binaryLog 6 === Some 3
let test = assert : binaryLog 7 === Some 3
let test = assert : binaryLog 8 === Some 4
let test = assert : binaryLog 9 === Some 4

-- (gcd m n) is the greatest common divisor of m and n. If either of m and n are zero, the result is None.


let example = Prelude.Optional.default Natural 999 (safeDivide 3 3)

in Natural/show example -- should return 1

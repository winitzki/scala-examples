-- Implement some arithmetical operations for Natural numbers.

-- Division with remainder, integer binary logarithm, integer square root, greatest common divisor, primality test.

let Prelude = https://prelude.dhall-lang.org/v20.2.0/package.dhall

-- Fold while an updater function returns a non-empty option, up to a given number of iterations.
let foldWhile: ∀(n: Natural) → ∀(res : Type) → ∀(succ : res → Optional res) → ∀(zero : res) → res =
    \(n: Natural) -> \(R: Type) -> \(succ: R -> Optional R) -> \(zero: R) ->
    let Acc: Type = { current: R, done: Bool }
    let update: Acc -> Acc = \(acc: Acc) -> if acc.done then acc else
    merge { Some = \(r: R) -> acc // {current = r}, None = acc // {done = True} } (succ acc.current)
    let init: Acc = { current = zero, done = False }
    let result: Acc = Natural/fold n Acc update init
    in
    result.current

-- Subtract 1 from 10 until the result is below 3. Max 20 iterations.
let test = assert: foldWhile 20 Natural (\(x: Natural) -> if Prelude.Natural.lessThan x 3 then None Natural else Some (Natural/subtract 1 x)) 10 === 2

-- Subtract y from x until the result becomes negative, count the number of steps and keep the last non-negative result.
let downwardSteps = \(x: Natural) -> \(y: Natural) ->
  let Acc: Type = { remainder: Natural, length: Natural }
  let initAcc: Acc = { remainder = x, length = 0 }
  let updateAcc: Acc -> Optional Acc = \(acc: Acc) ->
    if Prelude.Natural.lessThan acc.remainder y then None Acc
    else Some { remainder = Natural/subtract y acc.remainder, length = acc.length + 1 }
  in
  foldWhile (x + 1) Acc updateAcc initAcc

let test = assert : downwardSteps 15 4 === {remainder = 3, length = 3 }
let test = assert : downwardSteps 5 2 === {remainder = 1, length = 2 }
let test = assert : downwardSteps 4 2 === {remainder = 0, length = 2 }
let test = assert : downwardSteps 4 1 === {remainder = 0, length = 4 }
let test = assert : downwardSteps 4 5 === {remainder = 4, length = 0 }
let test = assert : downwardSteps 4 0 === {remainder = 4, length = 5 }


let div: Natural -> Natural -> Optional Natural = \(x: Natural) -> \(y: Natural) ->
    let result = downwardSteps x y
    in
    if Prelude.Natural.isZero y then None Natural else Some result.length

let test = assert : div 5 2 === Some 2
let test = assert : div 5 3 === Some 1
let test = assert : div 1 2 === Some 0
let test = assert : div 0 2 === Some 0
let test = assert : div 4 5 === Some 0
let test = assert : div 4 2 === Some 2
let test = assert : div 3 3 === Some 1
let test = assert : div 3 0 === None Natural
let test = assert : div 0 0 === None Natural
let test = assert : div 15 4 === Some 3
let test = assert : div 4 1 === Some 4

let rem: Natural -> Natural -> Optional Natural = \(x: Natural) -> \(y: Natural) ->
    let result = downwardSteps x y
    in
    if Prelude.Natural.isZero y then None Natural else Some result.remainder

let test = assert : rem 15 4 === Some 3
let test = assert : rem 5 2 === Some 1
let test = assert : rem 5 3 === Some 2
let test = assert : rem 0 2 === Some 0
let test = assert : rem 4 5 === Some 4
let test = assert : rem 4 2 === Some 0
let test = assert : rem 4 1 === Some 0
let test = assert : rem 3 3 === Some 0
let test = assert : rem 3 0 === None Natural
let test = assert : rem 0 0 === None Natural


-- (bitLength n) is how many times we need to divide n by 2 to obtain 0. This is 1 + log2 n.
let bitLength: Natural -> Natural = \(x: Natural) ->
  let Acc: Type = {current: Natural, count: Natural}
  let updateAcc: Acc -> Optional Acc = \(acc: Acc) ->
    if Prelude.Natural.isZero acc.current then None Acc else
    let divided = div acc.current 2
    let result: Acc = merge { Some = \(p: Natural) -> acc // {current = p, count = acc.count + 1}, None = acc // {count = 999} } divided
    in
    Some result

  let initAcc: Acc = {current = x, count = 0}
    let test = assert : updateAcc { current = 0, count = 1 } === None Acc
    let test = assert : updateAcc { current = 10, count = 2 } === Some { current = 5, count = 3 }
    let test = assert : updateAcc { current = 1, count = 0 } === Some { current = 0, count = 1 }
  let result = foldWhile (x + 1) Acc updateAcc initAcc
  in
  --if Prelude.Natural.isZero x then None Natural else Some
  result.count

let test = assert : bitLength 0 === 0
let test = assert : bitLength 1 === 1
let test = assert : bitLength 2 === 2
let test = assert : bitLength 3 === 2
let test = assert : bitLength 4 === 3
let test = assert : bitLength 5 === 3
let test = assert : bitLength 6 === 3
let test = assert : bitLength 7 === 3
let test = assert : bitLength 8 === 4
let test = assert : bitLength 9 === 4
let test = assert : bitLength 1023 === 10
let test = assert : bitLength 1024 === 11
let test = assert : bitLength 1025 === 11

-- (gcd m n) is the greatest common divisor of m and n. If either of m and n are zero, the result is also zero.
let gcd: Natural -> Natural -> Natural = \(x: Natural) -> \(y: Natural) ->
  let max = Prelude.Natural.max x y
  let min = Prelude.Natural.min x y
  let Acc: Type = { larger: Natural, smaller: Natural }
  let maxIterations = bitLength max
  let updateAcc: Acc -> Optional Acc = \(acc: Acc) ->
    if Prelude.Natural.isZero acc.smaller then None Acc
    else
      let rem = Prelude.Optional.default Natural 0 (rem acc.larger acc.smaller)
      in
      Some { larger = acc.smaller, smaller = rem }
  let initAcc: Acc = { larger = max, smaller = min }
  in
  if Prelude.Natural.isZero min then 0
  else (foldWhile maxIterations Acc updateAcc initAcc).larger

let test = assert : gcd 1 1 === 1
let test = assert : gcd 1 2 === 1
let test = assert : gcd 3 1 === 1
let test = assert : gcd 4 2 === 2
let test = assert : gcd 2 4 === 2
let test = assert : gcd 3 5 === 1
let test = assert : gcd 8 5 === 1
let test = assert : gcd 36 24 === 12
let test = assert : gcd 24 32 === 8
let test = assert : gcd 1024 32 === 32

-- (pow x y) is x to the power of y (which is x * x * ... * x repeated y times).
-- We define pow _ 0 = 1 including pow 0 0 = 1, but pow 0 y = 0 for other y.
let pow: Natural -> Natural -> Natural = \(x: Natural) -> \(y: Natural) ->
  Natural/fold y Natural (\(p: Natural) -> p * x) 1

let test = assert : pow 1 1 === 1
let test = assert : pow 1 4 === 1
let test = assert : pow 4 1 === 4
let test = assert : pow 2 3 === 8
let test = assert : pow 3 2 === 9
let test = assert : pow 0 1 === 0
let test = assert : pow 1 0 === 1
let test = assert : pow 0 2 === 0
let test = assert : pow 2 0 === 1
let test = assert : pow 0 0 === 1

-- (sqrt x) is the largest natural number k such that k * k <= x
let sqrt: Natural -> Natural = \(x: Natural) ->
  foldWhile x Natural (\(p: Natural) -> if Prelude.Natural.lessThanEqual (p * p) x then None Natural else Some (Natural/subtract 1 p)) x

let test = assert : sqrt 0 === 0
let test = assert : sqrt 1 === 1
let test = assert : sqrt 2 === 1
let test = assert : sqrt 4 === 2
let test = assert : sqrt 5 === 2
let test = assert : sqrt 25 === 5
let test = assert : sqrt 125 === 11
let test = assert : sqrt 256 === 16


in { div, rem, pow, bitLength, gcd, sqrt }

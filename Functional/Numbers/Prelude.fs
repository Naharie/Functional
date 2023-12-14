[<AutoOpen>]
module Functional.MathPrelude

open ExtendedNumerics
open Functional

type bigdecimal = BigDecimal

let inline clamp value min max =
    if value < min then min
    elif value > max then max
    else value

/// Returns the remainder from dividing the first item by the second item.
let inline rem n1 n2 = n1 % n2

/// The modulus operator.
/// Returns the mathematical result of n mod m.
let inline (%) n m =
    let r = n % m    
    if r < 0G then r + m else r
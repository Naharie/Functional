[<AutoOpen>]
module Functional.MathPrelude

open ExtendedNumerics
open Functional

type bigdecimal = BigDecimal

/// <summary>
/// Clamps the specified value to be at most <paramref name="max"/> and at least <paramref name="min"/>.
/// </summary>
/// <param name="value">The value to clamp.</param>
/// <param name="min">The minimum resulting value.</param>
/// <param name="max">The maximum resulting value.</param>
/// <returns>The clamped value.</returns>
let inline clamp value min max =
    if value < min then min
    elif value > max then max
    else value

/// <summary>
/// Returns the remainder from dividing the first item by the second item.
/// </summary>
/// <param name="number">The number to divide.</param>
/// <param name="divisor">The number to divide by.</param>
/// <returns>The remainder of the division.</returns>
let inline rem number divisor = number % divisor

/// <summary>
/// The modulus operator.
/// Returns the mathematical result of n mod m.
/// </summary>
/// <param name="n">The number to divide.</param>
/// <param name="m">The number to divide by.</param>
/// <returns><paramref name="n"/> mod <paramref name="m"/></returns>
let inline (%) n m =
    let r = n % m    
    if r < 0G then r + m else r
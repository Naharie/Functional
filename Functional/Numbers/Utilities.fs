[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Functional.Math

[<Measure>]
type radians
[<Measure>]
type degrees

/// Squares the given number.
let inline square x = x * x

/// Returns the distance between two points.
/// Always use distSquared when possible as it is more efficient.
let inline dist x1 y1 x2 y2 =
    square (x2 - x1) + square (y2 - y1)
    |> float
    |> sqrt
/// Returns the distance between two points squared.
let inline distSquared x1 y1 x2 y2 =
    square (x2 - x1) + square (y2 - y1)
    |> float

/// Returns a pair of the minimum and the maximum.
/// The first element is the minimum and the second element is the maximum.
let inline minMax a b =
    if a < b then a, b else b, a

/// Gets the rotation needed to turn from the first point (straight up) to face the second point.
let inline getRotation (x1: 'n) (y1: 'n) (x2: 'n) (y2: 'n): float<radians> =
    atan2
        (float x2 - float x1)
        (float y1 - float y2)
    |> LanguagePrimitives.FloatWithMeasure

/// Computes the greatest common divisor of two integers.
/// This is a faster algorithm that makes use of bitwise arithmetic.
let rec gcdInt a b =
    if a = b then a
    elif a = 0 then b
    elif b = 0 then a
    elif ~~~a &&& 1 > 0 then
        if b &&& 1 > 0 then
            gcdInt  (a >>> 1) b
        else
            gcdInt (a >>> 1) (b >>> 1) <<< 1
    elif ~~~b &&& 1 > 0 then
        gcdInt a (b >>> 1)
    elif a > b then
        gcdInt (a - b) b
    else
        gcdInt (b - a) a

/// Computes the greatest common divisor of two numbers.
let rec inline gcd a b =
    let mutable a = a
    let mutable b = b

    while a <> 0G && b <> 0G do
        let na, nb = b, a % b

        a <- na
        b <- nb

    if a = 0G then b else a
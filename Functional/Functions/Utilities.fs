[<Microsoft.FSharp.Core.AutoOpen>]
module Functional.UtilityFunctions

open System

let inline apply f x = f x
let inline apply2 f (x, y) = f x y

let inline applyBack f y x  = f x y
let inline applyBack3 f c b a = f a b c

let inline applyABC f a b c = f a b c
let inline applyACB f a c b = f a b c
let inline applyBAC f b a c = f a b c
let inline applyBCA f b c a = f a b c
let inline applyCAB f c a b = f a b c
let inline applyCBA f c b a = f a b c

let inline (@@) f b a = f a b
let inline (@@@) f c b a = f a b c

let inline constant x _ = x

/// <summary>
/// Prints the specified note to stdout and returns the input value.
/// </summary>
/// <param name="note">The note to display.</param>
/// <param name="x">The value to pass through.</param>
/// <returns>The value of <paramref name="x"/>.</returns>
let note (note: string) x =
    Console.WriteLine note
    x

/// <summary>
/// Prints the specified label and the input value to stdout and returns the input value.
/// </summary>
/// <param name="label">The note to display.</param>
/// <param name="x">The value to pass through.</param>
/// <returns>The value of <paramref name="x"/>.</returns>
let show label x =
    printfn $"%s{label}: %A{x}"
    x
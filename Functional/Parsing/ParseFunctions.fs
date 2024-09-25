[<AutoOpen>]
module Functional.Parsing

open System

/// <summary>
/// Wraps a TryParse function as a function that returns an option.
/// </summary>
/// <param name="f">The parser function to wrap.</param>
/// <param name="x">The value to parse.</param>
/// <returns>Some(value) if the parser succeeds and None if the parser fails.</returns>
let tryParse f = fun (x: string) ->
    match f x with
    | true, value -> Some value
    | _ -> None

/// Attempts to parse the specified string as an int32.
let tryInt = tryParse Int32.TryParse
/// Attempts to parse the specified string as an int64.
let tryInt64 = tryParse Int64.TryParse

/// Attempts to parse the specified string as a float.
let tryFloat = tryParse Double.TryParse
/// Attempts to parse the specified string as a float32.
let tryFloat32 = tryParse Single.TryParse
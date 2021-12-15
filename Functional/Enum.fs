[<RequireQualifiedAccess>]
module Functional.Enum

open System

/// Retrieves the values of the specified enumeration.
let getValues<'v, 't when 't : enum<'v>> () =
    Enum.GetValues (typeof<'t>) :?> 'v[]

/// <summary>Converts the string representation of the name or numeric value of one or more enumerated constants to an equivalent enumerated object.</summary>
/// <exception cref="System.ArgumentNullException">The value is null.</exception>
/// <exception cref="System.OverflowException">The value is outside the range of the underlying enum type.</exception>
let parse<'v, 't when 't: enum<'v>> (value: string) =
    Enum.Parse (typeof<'t>, value) :?> 'v

/// Attempts to convert the string representation of the name or numeric value of one or more enumerated constants to an equivalent enumerated object.
/// If the attempt succeeds then the result of the function will be Some(value) where "value" is the parsed enumeration.
/// If the attempt fails then the result of the function will be None.
let tryParse<'v, 't when 't: enum<'v> and 't :> ValueType and 't: (new: unit -> 't) and 't: struct> (value: string) =
    let mutable result = Unchecked.defaultof<'t>

    if Enum.TryParse<'t>(value, &result) then
        Some result
    else
        None
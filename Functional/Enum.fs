[<RequireQualifiedAccess>]
module Functional.Enum

open System

/// Retrieves the values of the specified enumeration.
let getValues<'v, 't when 't : enum<'v>> () =
    Enum.GetValues (typeof<'t>) :?> 'v[]

/// <summary>Converts the string representation of the name or numeric value of one or more enumerated constants to an equivalent enumerated object.</summary>
/// <param name="value">The value to parse as representing one of the enum's cases.</param>
/// <returns>The enum case matching the specified string value.</returns>
/// <exception cref="System.ArgumentNullException">The value is null.</exception>
/// <exception cref="System.OverflowException">The value is outside the range of the underlying enum type.</exception>
let parse<'v, 't when 't: enum<'v>> (value: string) =
    Enum.Parse (typeof<'t>, value) :?> 'v

/// <summary>
/// Attempts to convert the string representation of the name or numeric value of one or more enumerated constants to an equivalent enumerated object.
/// </summary>
/// <param name="value">The value to attempt to parse as representing one of the enum's cases.</param>
/// <returns>Either Some(parsedValue) or None if the parsing attempt was unsucesful.</returns>
let tryParse<'v, 't when 't: enum<'v> and 't :> ValueType and 't: (new: unit -> 't) and 't: struct> (value: string) =
    let mutable result = Unchecked.defaultof<'t>

    if Enum.TryParse<'t>(value, &result) then
        Some result
    else
        None
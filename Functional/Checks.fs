[<AutoOpen>]
module Functional.Checks

/// <summary>
/// Ensures that a value is not null by throwing a null reference exception to stop execution if it is null.
/// </summary>
/// <param name="name">The name of the value being checked.</param>
/// <param name="value">The value to be checked.</param>
let ensureNotNull name value = if isNull value then nullArg name
[<AutoOpen>]
module Functional.Checks

open System

/// <summary>
/// Ensures that a value is not null by throwing a null reference exception to stop execution if it is..
/// </summary>
/// <param name="name">The name of the value being checked.</param>
/// <param name="value">The value to be checked.</param>
let inline ensureNotNull name value = if isNull value then nullArg name

/// <summary>
/// Ensures that a value is not negative by throwing an invalid argument exception to stop execution if it is.
/// </summary>
/// <param name="name">The name of the value being checked.</param>
/// <param name="value">The value to be checked.</param>
let inline ensureNonNegative name value =
    if value < LanguagePrimitives.GenericZero then
        invalidArg name $"${name} must be non negative."
        
/// <summary>
/// Ensures that a value is within the bounds of a collection as set by min and max.
/// </summary>
/// <param name="name">The name of the value being checked.</param>
/// <param name="value">The value to be checked.</param>
/// <param name="min">The minimum allowed value (inclusive).</param>
/// <param name="max">The maximum allowed value (exclusive).</param>
let inline ensureInRange name value min max =
    if value < min || value >= max then
        invalidArg name $"%s{name} must be greater than or equal to %O{min} and less than ${max}."
[<Microsoft.FSharp.Core.RequireQualifiedAccess>]
module Functional.Binary

open System

/// <summary>
/// Converts the specified integer to binary.
/// </summary>
/// <param name="value">The integer to convert.</param>
/// <returns>The binary representation of <paramref name="value"/>.</returns>
let fromInt (value: int) =
    Convert.ToString(value, 2)
/// <summary>
/// Converts the binary representation of a number to a standard integer.
/// </summary>
/// <param name="value">The value to convert.</param>
/// <returns>The integer represented by the specified binary string.</returns>
/// <exception cref="System.ArgumentOutOfRangeException"><paramref name="value"/> is <see cref="System.String.Empty"/>.</exception>
/// <exception cref="System.FormatException"><paramref name="value"/> contains a character other than 1 and 0.</exception>
/// <exception cref="System.OverflowException">The integer represented by <paramref name="value"/> is less than <see cref="System.Int32.MinValue"/> or greater than <see cref="System.INt32.MaxValue"/>.</exception>
let toInt (value: string) =
    Convert.ToInt32(value, 2)
  
/// <summary>
/// Negates the specified binary value, passing through unknown characters unchanged.
/// </summary>
/// <param name="value">The binary value to negate.</param>
/// <returns>The negated binary value.</returns>
let negate (value: string) =
    value
    |> String.map (function
        | '0' -> '1'
        | '1' -> '0'
        | other -> other
    )
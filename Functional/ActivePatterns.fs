[<AutoOpen>]
module Functional.ActivePatterns

/// <summary>
/// Returns the specified default value if the optional value is None.
/// </summary>
/// <param name="default value">The fallback value to be provided when <paramref name="optional value" /> is None.</param>
/// <param name="optional value">The value to check and unwrap.</param>
/// <returns>The value contained by <paramref name="optional value"/> or the value of <paramref name="default value"/> if <paramref name="optional value"/> was None.</returns>
let (|Default|) ``default value`` ``optional value`` =
    Option.defaultValue ``default value`` ``optional value``

/// <summary>
/// Groups the specified value with the input as a tuple.
/// </summary>
/// <param name="value">The value to pair with the input arg.</param>
/// <param name="input">The input arg that will have the value paired with it.</param>
/// <returns>A pair of the specified value and input arg.</returns>
let (|Pair|) value input = value, input

/// Equality represented as an active pattern.
/// Useful when matching against non-trivial value representations, such as big integers.

/// <summary>
/// Equality represented as an active pattern.
/// Useful when matching against non-trivial value representations such as big integers.
/// </summary>
/// <param name="value">The value to check against.</param>
/// <param name="input">The value to check.</param>
/// <returns>Whether the two values were equal</returns>
let (|Is|) value input =
    if value = input then Some () else None
[<AutoOpen>]
module Functional.ActivePatterns

open System.Collections.Generic

/// Returns the specified default value if the optional value is None.
let (|Default|) ``default value`` ``optional value`` =
    Option.defaultValue ``default value`` ``optional value``

/// Groups the specified value with the input as a tuple.
let (|Pair|) value input = value, input

/// Equality represented as an active pattern.
/// Useful when matching against non trivial value representations, such as big integers.
let (|Is|) value input = value = input
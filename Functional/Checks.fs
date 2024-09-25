[<AutoOpen>]
module Functional.Checks

/// Ensures that a value is not null, throwing a null reference exception if it is.
let ensureNotNull name value = if isNull value then nullArg name
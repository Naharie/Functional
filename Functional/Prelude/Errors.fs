[<AutoOpen>]
module Functional.Errors

open System

/// Raises an ArgumentOutOfRangeException.
let argumentOutOfRange argumentName (message: string) =
    raise (ArgumentOutOfRangeException (argumentName, message))

/// Raises an IndexOutOfRangeException.
let indexOutOfRange message =
    raise (IndexOutOfRangeException message)

/// Raises a NotSupportedException.
let notSupported message =
    raise (NotSupportedException message)
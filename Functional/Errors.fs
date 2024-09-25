[<AutoOpen>]
module Functional.Errors

open System
open Functional.Exceptions

/// <summary>Raises an ArgumentOutOfRangeException.</summary>
/// <param name="argumentName">The name of the argument that was out of range.</param>
/// <param name="message">A message explaining the exception, usually detailing how the argument was out of range.</param>
let inline argumentOutOfRange argumentName (message: string) =
    raise (ArgumentOutOfRangeException (argumentName, message))

/// <summary>
/// Raises an IndexOutOfRangeException.
/// </summary>
/// <param name="message">A message explaining the exception.</param>
let inline indexOutOfRange message =
    raise (IndexOutOfRangeException message)

/// <summary>
/// Raises a NotSupportedException.
/// </summary>
/// <param name="message">A message explaining the exception.</param>
let inline notSupported message =
    raise (NotSupportedException message)
    
/// <summary>
/// Raises a NoValidItemsException
/// </summary>
/// <param name="message">A message explaining the exception.</param>
let inline noValidItems message =
    raise (NoValidItemException message)
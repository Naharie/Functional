[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Functional.Errors

open System
open Functional.Exceptions

/// <summary>Raises an ArgumentOutOfRangeException.</summary>
/// <param name="argumentName">The name of the argument that was out of range.</param>
/// <param name="message">A message explaining the exception, usually detailing how the argument was out of range.</param>
/// <exception cref="System.ArgumentOutOfRangeException">The exception being raised</exception>
let inline argumentOutOfRange argumentName (message: string) =
    raise (ArgumentOutOfRangeException (argumentName, message))

/// <summary>
/// Raises an IndexOutOfRangeException
/// </summary>
/// <param name="message">A message explaining the exception.</param>
/// <exception cref="System.IndexOutOfRangeException">The exception being raised.</exception>
let inline indexOutOfRange message = raise (IndexOutOfRangeException message)

/// <summary>
/// Raises a NotSupportedException.
/// </summary>
/// <param name="message">A message explaining the exception.</param>
/// <exception cref="System.NotSupportedException">The exception being raised.</exception>
let inline notSupported message =
    raise (NotSupportedException message)
    
/// <summary>
/// Raises a NoValidItemException
/// </summary>
/// <param name="message">A message explaining the exception.</param>
/// <exception cref="Functional.NoValidItemException">The exception being raised.</exception>
let inline noSuchItem message = raise (NoSuchItemException message)
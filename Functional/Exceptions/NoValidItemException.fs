namespace Functional.Exceptions

open System
open System.Collections.Generic

/// <summary>
/// The exception thrown when no items were found in a given collection that matched the specified predicate.
/// </summary>
/// <param name="message">The error message that explains the reason for the exception.</param>
/// <param name="innerException">The exception that is the cause of the current exception.</param>
type NoValidItemException(message: string, innerException: Exception) =
    inherit KeyNotFoundException(message, innerException)
    
    new(message) = NoValidItemException(message, null)
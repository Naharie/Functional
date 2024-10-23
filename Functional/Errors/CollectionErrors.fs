module Functional.Errors.CollectionErrors

open System
open Functional.Exceptions
    
/// <summary>
/// Raises an ArgumentException explaining that the collection was empty.
/// </summary>
/// <exception cref="System.ArgumentException">The exception being raised.</exception>
let inline invalidArgCollectionWasEmpty () = raise (ArgumentException "The input collection was empty.")

/// <summary>
/// Raises an IndexOutOfRangeException with a default message (index must be &gt;= 0 and &lt;= size of collection).
/// </summary>
/// <exception cref="System.IndexOutOfRangeException">The exception being raised.</exception>
let inline indexOutOfRangeMustBeWithinCollection () = indexOutOfRange "The index must be non-negative and less than the size of the collection."

/// <summary>
/// Raises an ArgumentException explaining that the collections were not of equal length.
/// </summary>
/// <exception cref="System.ArgumentException">The exception being raised.</exception>
let inline invalidArgNotOfEqualLength name = raise (ArgumentException $"The two %s{name}s were not of equal length.")
[<RequireQualifiedAccess>]
module Functional.Collections.Vec

// Core

/// <summary>
/// Returns the length of the specified vector.
/// </summary>
/// <param name="vec">The vector to fetch the length of.</param>
/// <returns>The length of the specified vector.</returns>
let length (vec: _ vec) = vec.Length

/// <summary>Returns the item at the specified index.</summary>
/// <exception cref="T:System.IndexOutOfRangeException">Thrown when the index is less than zero, greater than or eqaul to the size of the vector, or the vector is empty.</exception>
/// <param name="index">The index of the item to fetch.</param>
/// <param name="vec">The vector to fetch the item from.</param>
/// <returns>The item at the specified index.</returns>
let item index (vec: _ vec) = vec.Item index
    

/// <summary>
/// Produces a sequence that enumerates all the values in the vector.
/// </summary>
/// <param name="vec">The vector to wrap as a sequence</param>
/// <returns>The vector wrapped as a sequence.</returns>
let rec toSeq vec =
    failwith "TODO"

// Generic

// Specific

// Unsorted
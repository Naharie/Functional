module Functional.Errors.CollectionErrors

open System
open Functional.Exceptions
    
module NotEnoughItems =
    /// <summary>
    /// Raises an ArgumentException explaining that the collection was empty.
    /// </summary>
    /// <exception cref="System.ArgumentException">The exception being raised.</exception>
    let inline collectionWasEmpty () = raise (ArgumentException "The input collection was empty.")

    /// <summary>
    /// Raises an IndexOutOfRangeException with a default message (index must be &gt;= 0 and &lt; size of collection).
    /// </summary>
    /// <exception cref="System.IndexOutOfRangeException">The exception being raised.</exception>
    let inline indexOutOfCollectionBounds () = indexOutOfRange "Index out of range: must be non-negative and less than the size of the collection."

    module NamedIndexOutOfRange =
        /// Raises an ArgumentOutOfRangeException explaining that the caller tried to skip more items than exist within the collection.
        /// </summary>
        /// <exception cref="System.ArgumentOutOfRangeException">The exception being raised.</exception>
        let inline skipCountExceedsCollectionSize name = argumentOutOfRange name "The number of items to skip exceeds the number of items in the collection."

        /// Raises an ArgumentOutOfRangeException explaining that the caller tried to take more items than exist within the collection.
        /// </summary>
        /// <exception cref="System.ArgumentOutOfRangeException">The exception being raised.</exception>
        let inline takeCountExceedsCollectionSize name = argumentOutOfRange name "The number of items to take exceeds the number of items in the collection."

module PredicationOnCollection =
    /// <summary>
    /// Raises an ArgumentException explaining that the collections were not of equal length.
    /// </summary>
    /// <exception cref="System.ArgumentException">The exception being raised.</exception>
    let inline notOfEqualLength name = raise (ArgumentException $"The two %s{name}s were not of equal length.")

module PredicationOnItems =
    /// <summary>
    /// Raises a NoSuchItemException explaining that no item matching the predicate was found in the collection.
    /// </summary>
    /// <exception cref="Functional.Exceptions.NoSuchItemException">The exception being raised.</exception>
    let inline noMatchingItem() = noSuchItem "An element matching the predicate was not found in the collection."
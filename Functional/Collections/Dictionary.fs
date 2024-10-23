[<AutoOpen>]
module Functional.Dictionary

open System.Collections.Generic

type Dictionary<'tkey, 'tvalue> with
    /// <summary>
    /// Updates the value associated with the specified key using the given mapping function.
    /// </summary>
    /// <param name="key">The key to lookup.</param>
    /// <param name="mapping">The mapping to apply to the value.</param>
    member this.Change key mapping =
        this[key] <- mapping (this.TryFind key)
        
    /// <summary>
    /// Updates the value associated with the specified key using the given mapping function.
    /// </summary>
    /// <param name="key">The key to lookup.</param>
    /// <param name="mapping">The mapping to apply to the value.</param>
    member this.ChangeV key mapping =
        this[key] <- mapping (this.TryFindV key)
    
    /// <summary>Gets the value associated with the specified key.</summary>
    /// <param name="key">The key to lookup.</param>
    /// <returns>The value the key is mapped to or <c>None</c> if the dictionary does not contain the key.</returns>
    member this.TryFind key =
        if isNull (box key) then
            None
        else
            let mutable result = Unchecked.defaultof<'tvalue>

            if this.TryGetValue (key, &result) then Some result
            else None
            
    /// <summary>Gets the value associated with the specified key.</summary>
    /// <param name="key">The key to lookup.</param>
    /// <returns>The value the key is mapped to or <c>ValueNone</c> if the dictionary does not contain the key.</returns>
    member this.TryFindV key =
        if isNull (box key) then
            ValueNone
        else
            let mutable result = Unchecked.defaultof<'tvalue>

            if this.TryGetValue (key, &result) then ValueSome result
            else ValueNone
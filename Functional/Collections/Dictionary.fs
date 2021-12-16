[<AutoOpen>]
module Functional.Dictionary

open System.Collections.Generic

type Dictionary<'tkey, 'tvalue> with
    /// <summary>Gets the value associated with the specified key.</summary>
    /// <exception cref="System.ArgumentNullException">key is null</exception>
    member this.TryFind key =
        let mutable result = Unchecked.defaultof<'tvalue>

        if this.TryGetValue (key, &result) then Some result
        else None
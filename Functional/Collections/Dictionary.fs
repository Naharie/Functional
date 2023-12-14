[<AutoOpen>]
module Functional.Dictionary

open System.Collections.Generic

type Dictionary<'tkey, 'tvalue> with
    /// <summary>Gets the value associated with the specified key.</summary>
    member this.TryFind key =
        if isNull (box key) then
            None
        else
            let mutable result = Unchecked.defaultof<'tvalue>

            if this.TryGetValue (key, &result) then Some result
            else None
[<RequireQualifiedAccess>]
module Functional.Set

/// Creates a new set containing the results of applying the function to each item in the original set.
let collect (mapping: 't -> Set<'u>) (set: Set<'t>) =
    set
    |> Set.toArray
    |> Array.map mapping
    |> Set.unionMany
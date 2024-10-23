[<RequireQualifiedAccess>]
module Functional.Set

/// <summary>
/// Creates a new set containing the merged results of all the sets produced by the application of the mapping function to each element of the original set.
/// </summary>
/// <param name="mapping">The mapping function to apply.</param>
/// <param name="set">The set to apply the function to.</param>
/// <returns>The merger of the results of the mapping function.</returns>
let collect (mapping: 't -> Set<'u>) (set: Set<'t>) =
    set
    |> Set.toArray
    |> Array.map mapping
    |> Set.unionMany
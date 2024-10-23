[<RequireQualifiedAccess>]
module Functional.Map

/// <summary>
/// Returns a new map that contains all the pairs from both maps, where keys from the second map overwrite those from the first.
/// </summary>
/// <param name="map1">The map to merge onto.</param>
/// <param name="map2">The map to merge from.</param>
/// <returns>A new map that contains all the pairs from both maps, where keys from the second map overwrite those from the first.</returns>
let merge (map1: Map<_, _>) (map2: Map<_, _>) =
    Map.fold (fun map key value ->
        map |> Map.add key value
    ) map1 map2

/// <summary>
/// Returns a new map that contains all the pairs from both maps, where conflicts are resolved by the specified merger function.
/// </summary>
/// <param name="merger">The function used to resolve key conflicts.</param>
/// <param name="map1">The first map.</param>
/// <param name="map2">The second map.</param>
/// <returns>A new map that contains all the pairs from both maps</returns>
let mergeWith (merger: 't -> 't -> 't) (map1: Map<_, _>) (map2: Map<_, _>) =
    Map.fold (fun map key value ->
        match map |> Map.tryFind key with
        | Some existingValue ->
            map |> Map.add key (merger existingValue value)
        | None ->
            map |> Map.add key value

    ) map1 map2

/// <summary>
/// Returns a new map that contains all the key value pairs from all the maps in the sequence.
/// Keys from maps later in the sequence overwrite those from maps earlier in the sequence.
/// </summary>
/// <param name="maps">The maps to merge.</param>
/// <returns>The resulting merged map.</returns>
let mergeMany (maps: #seq<Map<_, _>>) =
    maps
    |> Seq.fold merge Map.empty

/// <summary>
/// Adds all the items from the sequence to the map.
/// </summary>
/// <param name="items">The items to add.</param>
/// <param name="map">The map to add the values to.</param>
/// <returns>The resulting map.</returns>
let addMany (items: #seq<('key * 'value)>) map =
    items
    |> Seq.fold (fun map (key, value) ->
        Map.add key value map
    ) map

/// <summary>
/// Removes all the items in the sequence from the map.
/// </summary>
/// <param name="items">The items to remove.</param>
/// <param name="map">The map to remove the values from.</param>
/// <returns>The resulting map.</returns>
let removeMany (items: #seq<'key>) (map: Map<'key, 'value>) =
    items
    |> Seq.fold (fun map key ->
        Map.remove key map
    ) map

/// <summary>
/// Adds the specified entry to the map only if an entry with the same key does not already exist.
/// </summary>
/// <param name="key">The key for the entry.</param>
/// <param name="value">The value for the entry.</param>
/// <param name="map">The map to add the value to.</param>
/// <returns>The resulting map.</returns>
let tryAdd (key: 'key) (value: 'value) (map: Map<'key, 'value>) =
    map
    |> Map.change key (fun previous ->
        previous
        |> Option.defaultValue value
        |> Some
    )
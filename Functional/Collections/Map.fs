[<RequireQualifiedAccess>]
module Functional.Map

/// Returns a new map that contains all the pairs from both maps, where keys from the second map overwrite those from map 1.
let merge (map1: Map<_, _>) (map2: Map<_, _>) =
    map1
    |> Map.fold (fun map key value ->
        map |> Map.add key value
    ) map2

/// Returns a new map that contains all the key value pairs from all the maps in the sequence.
/// Keys from maps later in the sequence overwrite those from earlier maps.
let mergeMany (maps: #seq<Map<_, _>>) =
    maps
    |> Seq.fold merge Map.empty

/// Adds all the items from the sequence to the map.
let addMany (items: #seq<('key * 'value)>) map =
    items
    |> Seq.fold (fun map (key, value) ->
        Map.add key value map
    ) map

/// Removes all the items in the sequence from the map.
let removeMany (items: #seq<'key>) (map: Map<'key, 'value>) =
    items
    |> Seq.fold (fun map key ->
        Map.remove key map
    ) map

/// Adds the specified entry to the map only if an entry with the same key does not already exist.
let tryAdd (key: 'key) (value: 'value) (map: Map<'key, 'value>) =
    map
    |> Map.change key (fun previous ->
        previous
        |> Option.defaultValue value
        |> Some
    )
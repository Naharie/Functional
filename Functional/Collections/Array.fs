[<RequireQualifiedAccess>]
module Functional.Array

open System
open Functional

open System.Collections
open System.Collections.Generic

/// Creates an array of arrays with the specified dimensions intialized with the specified function.
let table inner outer initializer =
    Array.init outer (fun _ -> Array.init inner initializer)

/// Combines map and scan.
let mapScan action state items =
    items
    |> Array.scan (fun (state, _) -> action state) (state, Unchecked.defaultof<'t>)
/// Combines map and scanBack.
let mapScanBack action state items =
    Array.scanBack (fun item (state, _) -> action item state) items (state, Unchecked.defaultof<'t>)

/// Converts an untyped IEnumerator to an object array.
let fromUntypedEnumerator (enumerator: IEnumerator) =
    Array.unfold (fun () ->
        if enumerator.MoveNext() then
            Some (enumerator.Current, ())
        else
            None
    ) ()
/// Converts an IEnumerator<'t> to a 't array.
let fromEnumerator (enumerator: IEnumerator<'t>) =
    Array.unfold (fun () ->
        if enumerator.MoveNext() then
            Some (enumerator.Current, ())
        else
            None
    ) ()

/// Converts a loose collection like XmlNodeList
let inline fromLooseCollection< ^c, ^t when ^c : (member Count: int) and ^c : (member Item: int -> ^t) > (collection: ^c) =
    let count = (^c : (member Count: int) collection)
    
    Array.init count (fun index ->
        (^c : (member Item: int -> ^t) (collection, index))
    )

/// Acting as a combination of map and choose, the resulting collection contains the elements from the original array for which the replacement function returned none.
/// If the replacement function returned Some(x) instead, then the value of x replaces the original element from the collection.
let replace replacement array =
    array
    |> Array.map (fun item ->
        replacement item
        |> Option.defaultValue item
    )
/// Creates a new array with the value at the specified index replaced with the specified value.
/// If the index is out of range, then the operation does nothing.
let updateAt index value array =
    let result = Array.copy array
    result.[index] <- value
    result

/// Applies the specified folding function to the array as long as the state is not None.
let foldWhile folder state array =
    let rec loop (state: 'state) index =
        if index >= Array.length array then
            state
        else
            let newState = folder state array.[index]

            match newState with
            | Some state -> 
                loop state (index + 1)
            | None -> state

    loop state 0
/// Applies the specified folding function to the array as long as the state is not None.
let foldBackWhile folder array state =
    let rec loop state index =
        if index < 0 then
            state
        else
            let newState = folder (Array.item index array) state

            match newState with
            | Some state -> loop state (index - 1)
            | None -> state

    loop state (Array.length array - 1)

/// Performs a standard fold unless the folding function returns none, in which case the overall function returns none.
let tryFold folder state array =
    let rec loop (state: 'state) index =
        if index >= Array.length array then
            Some state
        else
            let newState = folder state array.[index]

            match newState with
            | Some state -> loop state (index + 1)
            | None -> None

    loop state 0
/// Performs a standard fold back unless the folding function returns none, in which case the overall function returns none.
let tryFoldBack folder array state =
    let rec loop state index =
        if index < 0 then
            Some state
        else
            let newState = folder (Array.item index array) state

            match newState with
            | Some state -> loop state (index - 1)
            | None -> None

    loop state (Array.length array - 1)

/// Combines fold and filter into a single function that threads the state object through the filtering process.
let thresh folder (state: 'state) (array: 't[]) =
    let mutable state = state

    let filtered =
        array
        |> Array.filter (fun item ->
            let keep, newState = folder state item

            state <- newState
            keep
        )

    filtered, state
/// Combines foldBack and filter into a single function that threads the state object through the filtering process.
let threshBack folder array state =
    let mutable state = state
    let length = Array.length array
    let mask = BitArray length

    for index in length - 1..-1..0 do
        let keep, newState = folder array.[index]
        state <- newState
        mask.[index] <- keep

    let filtered =
        let mutable index = -1

        array
        |> Array.filter (fun _ ->
            index <- index + 1
            mask.[index]
        )

    filtered, state

/// Combines fold and choose into a single function that threads the state object through the filtering process.
let winnow folder state array =
    let mutable state = state

    let filtered =
        array
        |> Array.choose (fun item ->
            let result, newState = folder state item

            state <- newState
            result
        )

    filtered, state
/// Combines fold and choose into a single function that threads the state object through the filtering process.
let winnowBack folder (array: 't[]) (state: 'state) =
    let mutable state = state
    let length = Array.length array
    let mask = Array.zeroCreate array.Length

    for index in length - 1..-1..0 do
        let result, newState = folder array.[index]
        state <- newState
        mask.[index] <- result

    (Array.choose id mask), state

/// Returns the first element for which the given predicate returns "true".
/// If there is no such element then a KeyNotFoundException is raised instead.
let findi predicate (array: 't[]) =
    let mutable index = -1
    let mutable go = true
    
    while index < array.Length && go do
        if predicate index array.[index] then
            go <- false
        else
            index <- index + 1
    
    if index = array.Length then
        raise (KeyNotFoundException "An element matching the predicate was not found in the array.")
    else
        array.[index]

/// Returns the last element for which the given predicate returns "true".
/// If there is no such element then a KeyNotFoundException is raised instead.
let findBacki predicate (array: 't[]) =
    let mutable index = array.Length - 1
    let mutable go = true
    
    while index > -1 && go do
        if predicate index array.[index] then
            go <- false
        else
            index <- index - 1
    
    if index = -1 then
        raise (KeyNotFoundException "An element matching the predicate was not found in the array.")
    else
        array.[index]

/// Returns the first element for which the given predicate returns "true".
/// If there is no such element then None is returned instead.
let tryFindi predicate (array: 't[]) =
    let mutable index = -1
    let mutable go = true
    
    while index < array.Length && go do
        if predicate index array.[index] then
            go <- false
        else
            index <- index + 1
    
    if index = array.Length then
        None
    else
        Some array.[index]

/// Returns the last element for which the given predicate returns "true".
/// If there is no such element then None is returned instead.
let tryFindBacki predicate (array: 't[]) =
    let mutable index = array.Length - 1
    let mutable go = true
    
    while index > -1 && go do
        if predicate index array.[index] then
            go <- false
        else
            index <- index - 1
    
    if index = -1 then
        None
    else
        Some array.[index]

/// Returns the first element for which the given predicate returns Some x.
/// If there is no such element then a KeyNotFoundException is raised instead.
let picki (chooser: int -> 't -> 'u option) (array: 't[]) =
    let mutable index = -1
    let mutable go = true
    let mutable result = Unchecked.defaultof<'u>
    
    while index < array.Length && go do
        match chooser index array.[index] with
        | Some value ->
            result <- value
            go <- false
        | None ->
            index <- index + 1

    if index = array.Length then
        raise (KeyNotFoundException "An element matching the predicate was not found in the array.")
    else
        result

/// Returns the first element for which the given predicate returns Some x.
/// If there is no such element then None is returned instead.
let tryPicki (chooser: int -> 't -> 'u option) (array: 't[]) =
    let mutable index = -1
    let mutable go = true
    let mutable result = None
    
    while index < array.Length && go do
        match chooser index array.[index] with
        | Some value ->
            result <- Some value
            go <- false
        | None ->
            index <- index + 1

    if index = array.Length then
        None
    else
        result

/// For each element apply the given function, concatenate all the results and returned the combined result.
let collecti mapping array =
    [|
        let mutable index = 0
        
        for item in array do
            yield! mapping index item
            index <- index + 1
    |]

/// Returns a new collection containing only the elements of the collection for which the given predicate returns true.
let filteri predicate array =
    [|
        let mutable index = 0
        
        for item in array do
            if predicate index item then
                yield item

            index <- index + 1
    |]
/// Returns a new collection containing only the elements of the collection for which the given predicate returns Some;
let choosei predicate array =
    [|
        let mutable index = 0
        
        for item in array do
            match predicate index item with
            | Some value -> yield value
            | None ->
                index <- index + 1
    |]

/// Removes all instances of the specified item from the array.
let without (item: 't) (array: 't[]) =
    array
    |> Array.filter ((<>) item)
/// Removes the item a the specified index (if it is within the array).
let removeAt (index: int) (array: 't[]) =
    if index < 0 || index >= array.Length then
        Array.copy array
    else
        let result = Array.zeroCreate (array.Length - 1)
        
        Array.Copy (array, result, index)
        Array.Copy (array, index + 1, result, index, array.Length - index - 1)
        
        result

/// Adds the item to the beginning of the collection.
let prependItem (item: 't) (array: 't array) =
    Array.append [| item |] array
/// Adds the item to the end of the collection.
let appendItem (item: 't) (array: 't array) =
    Array.append array [| item |]

/// All possible insertions of a given item into the array.
/// insertions 4 [ 1; 2; 3 ] -> [ 4; 1; 2; 3 ]; [ 1; 4; 2; 3 ]; [ 1; 2; 4; 3 ]; [ 1; 2; 3; 4 ]
let insertions x items =
    let size = Array.length items + 1
    
    Array.init size (fun insertIndex ->
        Array.init size (fun index ->
            if index = insertIndex then
                x
            else
                let offsetIndex = if index < insertIndex then index else index - 1
                items.[offsetIndex]
        )
    )

/// Swaps the specified two items of the array.
/// If either index is out of bounds than the operation does nothing.
// This version mutably updates the passed in array.
let swapInline a b items =
    let length = Array.length items

    if a >= 0 && a < length && b >= 0 && b < length then
        let temp = items.[a]
        items.[a] <- items.[b]
        items.[b] <- temp

    items
/// Swaps the specified two items of the array.
/// If either index is out of bounds than the operation does nothing.
let swap a b items =
    swapInline a b (Array.copy items)

/// Reverses the specified section of the array.
/// If an index is out of bounds then it will be clamped to the closest bound.
let reverseSectionInline start stop items =
    let length = Array.length items
    
    let start = clamp start 0 (length - 1)
    let stop = clamp stop 0 (length - 1)

    let size = stop - start + 1
    let steps = (size / 2) - (if size % 2 = 0 then 0 else 1)

    for index in 0..steps do
        swapInline (start + index) (stop - index) items |> ignore
    
    items
/// Reverses the specified section of the array.
/// If an index is out of bounds then it will be clamped to the closest bound.
let reverseSection start stop items =
    reverseSectionInline start stop (Array.copy items)

// Returns all pairs where both items are from unique indexes.
let pairs array =
    array
    |> collecti (fun outerIndex a ->
        array
        |> choosei (fun innerIndex b ->
            if outerIndex = innerIndex then
                None
            else
                Some (a, b)
        )
    )

/// Splits the input array based on the specified rule function.
/// The item that is split on is not included in the results.
let splitBy (rule: 't -> bool) (array: 't[]) =
    let rec loop groups previous index =
        if index >= array.Length then
            if index > previous then
                List.rev (array.[previous..index - 1] :: groups)
                |> List.toArray
            else
                groups
                |> List.rev
                |> List.toArray
        else
            if rule array.[index] then
                loop (array.[previous..index - 1] :: groups) (index + 1) (index + 1)
            else
                loop groups previous (index + 1)

    loop [] 0 0

/// Splits the input array based on the specified rule function.
/// The item that is split on is included in the results as the first item of each section.
let chunkBy (rule: 't -> bool) (array: 't[]) =
    let rec loop groups previous index =
        if index >= array.Length then
            if index > previous then
                List.rev (array.[previous..index] :: groups)
                |> List.toArray
            else
                groups
                |> List.rev
                |> List.toArray
        else
            if rule array.[index] then
                loop (array.[previous..index - 1] :: groups) index (index + 1)
            else
                loop groups previous (index + 1)

    loop [] 0 0
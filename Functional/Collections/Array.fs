[<RequireQualifiedAccess>]
module Functional.Array

open System
open Functional

open System.Collections
open System.Collections.Generic

// Generic

/// <summary>
/// Returns the first element of the array or <c>ValueNone</c> if the array is empty.
/// </summary>
/// <param name="array">The input array.</param>
/// <returns>The first element of the array or <c>ValueNone</c> if the array is empty.</returns>
let tryHeadV (array: 't[]) =
    if array.Length = 0 then ValueNone
    else ValueSome array[0]

/// <summary>
/// Returns the index of the last item for which the predicate returns true or <c>ValueNone</c> if there is no such item.
/// </summary>
/// <param name="predicate">The function to evaluate each item with.</param>
/// <param name="array">The input array.</param>
/// <returns>The index of the last item for which the predicate returns true or <c>ValueNone</c> if there is no such item.</returns>
let tryFindIndexBackV predicate (array: 't[]) =
    let rec loop index =
        if index < 0 then
            ValueNone
        elif predicate array[index] then
            ValueSome index
        else
            loop (index - 1)
    
    loop (array.Length - 1)

// tryHeadV, tryFindV, tryFindBackV, tryFindIndexV, tryLastV, pickV, tryPickV, chooseV, unfoldV, tryExactlyOneV, repeat

/// <summary>
/// Creates an array of arrays with the specified number of inner and outer items and initialized with the given function.
/// </summary>
/// <param name="inner">The number of items in each inner array.</param>
/// <param name="outer">The number of arrays in the outer array.</param>
/// <param name="initializer">The initializer function.</param>
let table inner outer initializer =
    Array.init outer (fun row -> Array.init inner (initializer row))

// Specific

/// Updates the array in place and returns it again.
let mapInline mapping (array: 't[]) =    
    for i in 0..array.Length do
        array[i] <- mapping array[i]
    
    array

// Unsorted

/// Combines map and scan.
let mapScan action state items =
    items
    |> Array.scan (fun (state, _) -> action state) (state, Unchecked.defaultof<'t>)
/// Combines map and scanBack.
let mapScanBack action state items =
    Array.scanBack (fun item (state, _) -> action item state) items (state, Unchecked.defaultof<'t>)

/// Converts an untyped IEnumerator to an object array.
let fromUntypedEnumerator (enumerator: IEnumerator) =
    let result = ResizeArray()
    
    while enumerator.MoveNext() do
        result.Add enumerator.Current

    result.ToArray()
/// Converts an IEnumerator<'t> to a 't array.
let fromEnumerator (enumerator: IEnumerator<'t>) =
    let result = ResizeArray()
    
    while enumerator.MoveNext() do
        result.Add enumerator.Current
        
    result.ToArray()

/// Converts a loose collection like XmlNodeList
let inline fromCollectionLike< ^c, ^t when ^c : (member Count: int) and ^c : (member Item: int -> ^t) > (collection: ^c) =
    let count = (^c : (member Count: int) collection)
    Array.init count (fun index -> (^c : (member Item: int -> ^t) (collection, index)))

/// Replaces the specified value 'before' with the value 'after'.
let replace before after array =
    array
    |> Array.map (fun value -> if value = before then after else value)

/// Acting as a combination of map and choose, the resulting collection contains the elements from the original array for which the replacement function returned none.
/// If the replacement function returned Some(x) instead, then the value of x replaces the original element from the collection.
let replaceWith replacement array =
    array
    |> Array.map (fun item ->
        replacement item
        |> Option.defaultValue item
    )

/// Applies a function to each element and its index, threading an accumulator through the computation.
let foldi folder state (array: 't[]) =
    ensureNotNull "array" array

    let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(folder)
    let mutable result = state

    for i = 0 to array.Length - 1 do
        result <- f.Invoke(i, result, array.[i])

    result

/// Applies the specified folding function to the array as long as the state is not None.
let foldWhile folder state array =
    let rec loop (state: 'state) index =
        if index >= Array.length array then
            state
        else
            let newState = folder state array[index]

            match newState with
            | Done value -> value
            | Continue state -> 
                loop state (index + 1)

    loop state 0
/// Applies the specified folding function to the array as long as the state is not None.
let foldBackWhile folder array state =
    let rec loop state index =
        if index < 0 then
            state
        else
            let newState = folder (Array.item index array) state

            match newState with
            | Done value -> value
            | Continue state -> 
                loop state (index - 1)

    loop state (Array.length array - 1)

/// Performs a standard fold unless the folding function returns none, in which case the overall function returns none.
let tryFold folder state array =
    let rec loop (state: 'state) index =
        if index >= Array.length array then
            Some state
        else
            let newState = folder state array[index]

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
        let keep, newState = folder array[index]
        state <- newState
        mask[index] <- keep

    let filtered =
        let mutable index = -1

        array
        |> Array.filter (fun _ ->
            index <- index + 1
            mask[index]
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
        let result, newState = folder array[index]
        state <- newState
        mask[index] <- result

    (Array.choose id mask), state

/// Returns the first element for which the given predicate returns "true".
/// If there is no such element then a KeyNotFoundException is raised instead.
let findi predicate (array: 't[]) =
    let mutable index = -1
    let mutable go = true
    
    while index < array.Length && go do
        if predicate index array[index] then
            go <- false
        else
            index <- index + 1
    
    if index = array.Length then
        raise (KeyNotFoundException "An element matching the predicate was not found in the array.")
    else
        array[index]

/// Returns the last element for which the given predicate returns "true".
/// If there is no such element then a KeyNotFoundException is raised instead.
let findBacki predicate (array: 't[]) =
    let mutable index = array.Length - 1
    let mutable go = true
    
    while index > -1 && go do
        if predicate index array[index] then
            go <- false
        else
            index <- index - 1
    
    if index = -1 then
        raise (KeyNotFoundException "An element matching the predicate was not found in the array.")
    else
        array[index]

/// Returns the first element for which the given predicate returns "true".
/// If there is no such element then None is returned instead.
let tryFindi predicate (array: 't[]) =
    let mutable index = -1
    let mutable go = true
    
    while index < array.Length && go do
        if predicate index array[index] then
            go <- false
        else
            index <- index + 1
    
    if index = array.Length then
        None
    else
        Some array[index]

/// Returns the last element for which the given predicate returns "true".
/// If there is no such element then None is returned instead.
let tryFindBacki predicate (array: 't[]) =
    let mutable index = array.Length - 1
    let mutable go = true
    
    while index > -1 && go do
        if predicate index array[index] then
            go <- false
        else
            index <- index - 1
    
    if index = -1 then
        None
    else
        Some array[index]

/// Returns the first element for which the given predicate returns Some x.
/// If there is no such element then a KeyNotFoundException is raised instead.
let picki (chooser: int -> 't -> 'u option) (array: 't[]) =
    let mutable index = -1
    let mutable go = true
    let mutable result = Unchecked.defaultof<'u>
    
    while index < array.Length && go do
        match chooser index array[index] with
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
        match chooser index array[index] with
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
let collecti (mapping: int -> 't -> 'u array) (array: 't array) =
    [|
        let mutable index = 0
        
        for item in array do
            yield! mapping index item
            index <- index + 1
    |]

/// Returns a new collection containing only the elements of the collection for which the given predicate returns true.
let filteri predicate (array: 't array) =
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
            | None -> ()
            
            index <- index + 1
    |]

/// Removes all instances of the specified item from the array.
let without (item: 't) (array: 't[]) =
    array
    |> Array.filter ((<>) item)

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
                items[offsetIndex]
        )
    )

/// Swaps the specified two items of the array.
/// If either index is out of bounds than the operation does nothing.
/// This version mutably updates the passed in array.
let swapInline a b items =
    let length = Array.length items

    if a >= 0 && a < length && b >= 0 && b < length then
        let temp = items[a]
        items[a] <- items[b]
        items[b] <- temp

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

/// Splits the input sequence based on the specified rule function.
/// The item that is split on is not included in the results.
let splitBy predicate (sequence: 't[]) =
    [
        let buffer = ResizeArray()

        for item in sequence do
            if predicate item then
                yield buffer.ToArray()
                buffer.Clear()
            else
                buffer.Add item

        if buffer.Count > 0 then 
            yield buffer.ToArray()
    ]

let splitByOptions options predicate (sequence: 't[]) =
    [
        let buffer = ResizeArray()

        for item in sequence do
            if predicate item then
                match options with
                | DoNotIncludeSeparator ->
                    yield buffer.ToArray()
                    buffer.Clear()
                | IncludeSeparatorAsFirstElement ->
                    yield buffer.ToArray()
                    buffer.Clear()
                    buffer.Add item
                | IncludeSeparatorAsLastElement ->
                    buffer.Add item
                    yield buffer.ToArray()
                    buffer.Clear()
                | IncludeSeparatorAsOwnGroup ->
                    yield buffer.ToArray()
                    yield Array.singleton item
                    buffer.Clear()
            else
                buffer.Add item

        if buffer.Count > 0 then 
            yield buffer.ToArray()
    ]

/// Collapses an array according to the specified rule.
/// All pairs which the rule returns Some(combinedValue) are replaced by combinedValue.
/// This happens recursively for any new pairs created by the merging of a previous pair.
let collapse (rule: 't -> 't -> 't option) array =
    if Array.isEmpty array then
        Array.empty
    else
        [|
            let mutable state = array[0]
            let mutable index = 1
            
            while index < array.Length do
                match rule state array[index] with
                | Some nextState ->
                    state <- nextState
                | None ->
                    yield state
                    state <- array[index]
                    
                index <- index + 1

            yield state
        |]

/// Counts the number of items that match the specified rule.
let count rule (array: 't[]) =
    let mutable count = 0
    for item in array do if rule item then count <- count + 1
    count
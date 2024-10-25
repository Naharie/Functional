[<RequireQualifiedAccess>]
module Functional.Array

open System
open Functional

open System.Collections
open System.Collections.Generic

// Specific

/// <summary>
/// Applies the mapping function to each element in the array, mutating the original array and updating it with each resulting value.
/// </summary>
/// <param name="mapping">The mapping to apply to each element.</param>
/// <param name="array">The array to read and write to.</param>
/// <returns>The updated array.</returns>
/// <exception cref="System.NullArgumentException">The input array was null.</exception>
let mapInline mapping (array: 't[]) =
    ensureNotNull (nameof array) array
    
    for i in 0..array.Length do
        array[i] <- mapping array[i]
    
    array

// Generic

/// <summary>
/// Returns an array that repeats the given values <c>count</c> times.
/// </summary>
/// <param name="count">The number of times to repeat the values.</param>
/// <param name="array">The values to repeat.</param>
/// <returns>An array that repeats the given values <c>count</c> times.</returns>
/// <exception cref="System.ArgumentException"><c>count</c> is less than zero.</exception>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let repeat count (array: 't[]) =
    ensureNonNegative (nameof count) count
    ensureNotNull (nameof array) array
    
    let output = Array.zeroCreate (count * array.Length)
    
    for i in 0..(count - 1) do
        Array.Copy(array, 0, output, i * array.Length, array.Length)
    
    output

/// <summary>
/// Creates an array of arrays with the specified number of inner and outer items and initialized with the given function.
/// </summary>
/// <param name="inner">The number of items in each inner array.</param>
/// <param name="outer">The number of arrays in the outer array.</param>
/// <param name="initializer">The initializer function.</param>
/// <exception cref="System.ArgumentException"><c>inner</c> was less than zero.</exception>
/// <exception cref="System.ArgumentException"><c>outer</c> was less than zero.</exception>
let table inner outer initializer =
    ensureNonNegative (nameof inner) inner
    ensureNonNegative (nameof outer) outer
    
    Array.init outer (fun row -> Array.init inner (initializer row))

/// <summary>
/// Applies the mapping function to each element of the array, threading an accumulator through the computation, building a new array containing pairs of the state and the mapped item.
/// </summary>
/// <param name="mapping">The mapping to apply.</param>
/// <param name="state">The initial starting state.</param>
/// <param name="array">The input array.</param>
/// <returns>The resulting array.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let mapScan mapping (state: 'state) (array: 't[]) =
    array
    |> Array.scan (fun (state, _) -> mapping state) (state, Unchecked.defaultof<'t>)
/// <summary>
/// Applies the mapping function to each element of the array in reverse order, threading an accumulator through the computation, building a new array containing pairs of the state the and the mapped item.
/// </summary>
/// <param name="mapping">The mapping to apply.</param>
/// <param name="state">The initial starting state.</param>
/// <param name="array">The array to apply the function to.</param>
/// <returns>The resulting array.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let mapScanBack mapping state array =
    Array.scanBack (fun item (state, _) -> mapping item state) array (state, Unchecked.defaultof<'t>)

/// <summary>
/// Converts an untyped <c>IEnumerator</c> in an array of objects.
/// </summary>
/// <param name="enumerator">The enumerator to convert.</param>
/// <returns>The resulting array.</returns>
/// <exception cref="System.NullArgumentException"><c>enumerator</c> was null.</exception>
let fromUntypedEnumerator (enumerator: IEnumerator) =
    [| while enumerator.MoveNext() do yield enumerator.Current |]
/// <summary>
/// Converts an <c>IEnumerator&lt;'t&gt;</c> to an array of 't.
/// </summary>
/// <param name="enumerator">The enumerator to convert.</param>
let fromEnumerator (enumerator: IEnumerator<'t>) =
    [| while enumerator.MoveNext() do yield enumerator.Current |]

/// <summary>
/// Converts an object that acts like a collection (that is, has <c>Count</c> and <c>Item</c> defined) to an array.
/// </summary>
/// <param name="collection">The collection to convert.</param>
/// <returns>The resulting array.</returns>
let inline fromCollection< ^c, ^t when ^c : (member Count: int) and ^c : (member Item: int -> ^t) > (collection: ^c) =
    Array.init collection.Count collection.Item

/// <summary>
/// Replaces all instances of <c>before</c> in the array with <c>after</c>.
/// </summary>
/// <param name="before">The value to replace.</param>
/// <param name="after">The value to replace with.</param>
/// <param name="array">The input array.</param>
/// <returns>The resulting array.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let replace before after array =
    array
    |> Array.map (fun value -> if value = before then after else value)

/// <summary>
/// Applies a function to each element and its index, threading an accumulator through the computation.
/// </summary>
/// <param name="folder">The function to compute each state given the last.</param>
/// <param name="state">The initial starting state.</param>
/// <param name="array">The input array.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let foldi folder (state: 'state) (array: 't[]) =    
    ensureNotNull "array" array

    let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(folder)
    let mutable result = state

    for i = 0 to array.Length - 1 do
        result <- f.Invoke(i, result, array.[i])

    result

/// <summary>
/// Applies the specified folding function to each element of the array as long as the state is not <c>Done</c>.
/// </summary>
/// <param name="folder">The function to generate each state given the previous state.</param>
/// <param name="state">The initial state.</param>
/// <param name="array">The input array.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let foldWhile folder (state: 'state) (array: 't[]) =
    ensureNotNull (nameof array) array
    
    let rec go (state: 'state) index =
        if index >= Array.length array then
            state
        else
            let status, newState = folder state array[index]

            match status with
            | Done -> newState
            | Continue -> 
                go newState (index + 1)

    go state 0
/// <summary>
/// Applies the specified folding function to each element of the array in reverse order as long as the state is not <c>Done</c>.
/// </summary>
/// <param name="folder">The function to generate each state given the previous state.</param>
/// <param name="state">The initial state.</param>
/// <param name="array">The input array.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let foldBackWhile folder (array: 't[]) (state: 'state) =
    ensureNotNull (nameof array) array
    
    let rec loop state index =
        if index < 0 then
            state
        else
            let status, newState = folder (Array.item index array) state

            match status with
            | Done -> newState
            | Continue -> 
                loop newState (index - 1)

    loop state (Array.length array - 1)

/// <summary>
/// Performs a standard fold unless the folding function returns <c>None</c>, in which case the overall function returns <c>None</c>.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="array">The input array.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let tryFold folder state array =
    ensureNotNull (nameof array) array
    
    let rec loop (state: 'state) index =
        if index >= Array.length array then
            Some state
        else
            let newState = folder state array[index]

            match newState with
            | Some state -> loop state (index + 1)
            | None -> None

    loop state 0
/// <summary>
/// Performs a standard foldBack unless the folding function returns <c>None</c>, in which case the overall function returns <c>None</c>.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="array">The input array.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let tryFoldBack folder array state =
    ensureNotNull (nameof array) array
    
    let rec loop state index =
        if index < 0 then
            Some state
        else
            let newState = folder (Array.item index array) state

            match newState with
            | Some state -> loop state (index - 1)
            | None -> None

    loop state (Array.length array - 1)

/// <summary>
/// Combines fold and filter into a single function that threads the state object through the filtering process.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="array">The input array.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let thresh folder (state: 'state) (array: 't[]) =
    ensureNotNull (nameof array) array
    
    let mutable state = state

    let filtered =
        array
        |> Array.filter (fun item ->
            let keep, newState = folder state item
            state <- newState
            keep
        )

    filtered, state
/// <summary> 
/// Combines foldBack and filter into a single function that threads the state object through the filtering process.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="array">The input array.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let threshBack folder array state =
    ensureNotNull (nameof array) array
    
    let mutable state = state
    let length = Array.length array
    let mask = BitArray length

    for index in (length - 1)..(-1)..0 do
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

/// <summary>
/// Combines fold and choose into a single function that threads the state object through the filtering process.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="array">The input array.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let winnow folder state (array: 't[]) =
    ensureNotNull (nameof array) array
    
    let mutable state = state

    [|
        for item in array do
            let result, newState = folder state item
            state <- newState
            
            match result with
            | Some x -> yield x
            | None -> ()
    |], state
/// <summary>
/// Combines fold and choose into a single function that threads the state object through the filtering process.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="array">The input array.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let winnowBack folder (array: 't[]) (state: 'state) =
    ensureNotNull (nameof array) array
    
    let mutable state = state
    let length = Array.length array
    let mask = Array.zeroCreate array.Length

    for index in length - 1..-1..0 do
        let result, newState = folder array[index]
        state <- newState
        mask[index] <- result

    (Array.choose id mask), state

/// <summary>
/// Returns the first element for which the given predicate returns "true".
/// </summary>
/// <param name="predicate">The predicate to evaluate each item with.</param>
/// <param name="array">The input array.</param>
/// <returns>The first element for which the given predicate returns "true".</returns>
/// <exception cref="Functional.NoSuchItemException">The predicate did not evaluate to true for any items.</exception>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let findi predicate (array: 't[]) =
    ensureNotNull (nameof array) array
    
    let rec go index =
        if index >= array.Length then
            noSuchItem "An element matching the predicate was not found in the array."
        elif predicate index array[index] then
            array[index]
        else
            go (index + 1)
        
    go 0

/// <summary>
/// Returns the last element for which the given predicate returns "true".
/// </summary>
/// <param name="predicate">The predicate to evaluate each item with.</param>
/// <param name="array">The input array.</param>
/// <returns>The first element for which the given predicate returns "true".</returns>
/// <exception cref="Functional.NoSuchItemException">The predicate did not evaluate to true for any items.</exception>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let findBacki predicate (array: 't[]) =
    ensureNotNull (nameof array) array
    
    let rec go index =
        if index < 0 then
            noSuchItem "An element matching the predicate was not found in the array."
        elif predicate index array[index] then
            array[index]
        else
            go (index - 1)
        
    go (array.Length - 1)

/// <summary>
/// Returns the first element for which the given predicate returns "true" or <c>None</c> if there is no such element.
/// </summary>
/// <param name="predicate">The predicate to check each item with.</param>
/// <param name="array">The input array.</param>
/// <returns>The first element for which the given predicate returns "true" or <c>None</c> if there is no such element.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let tryFindi predicate (array: 't[]) =
    ensureNotNull (nameof array) array
    
    let rec go index =
        if index >= array.Length then
            None
        elif predicate index array[index] then
            Some array[index]
        else
            go (index + 1)
        
    go 0

/// <summary>
/// Returns the last element for which the given predicate returns "true" or <c>None</c> if there is no such element.
/// </summary>
/// <param name="predicate">The predicate to check each item with.</param>
/// <param name="array">The input array.</param>
/// <returns>The last element for which the given predicate returns "true" or <c>None</c> if there is no such element.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let tryFindBacki predicate (array: 't[]) =
    ensureNotNull (nameof array) array
    
    let rec go index =
        if index < 0 then
            None
        elif predicate index array[index] then
            Some array[index]
        else
            go (index - 1)
        
    go (array.Length - 1)

/// <summary>
/// Returns the first element for which the given predicate returns <c>Some(x)</c>.
/// </summary>
/// <param name="chooser">The choice function to apply to each element.</param>
/// <param name="array">The input array.</param>
/// <exception cref="Functional.NoSuchItemException">The choice function returned <c>None</c> for all items.</exception>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let picki (chooser: int -> 't -> 'u option) (array: 't[]) =
    ensureNotNull (nameof array) array
    
    let rec go index =
        if index >= array.Length then
            noSuchItem "An element matching the predicate was not found in the array."
        else
            match chooser index array[index] with
            | Some value -> value
            | None -> go (index + 1)

    go 0

/// Returns the first element for which the given predicate returns Some x.
/// If there is no such element then None is returned instead.
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let tryPicki (chooser: int -> 't -> 'u option) (array: 't[]) =
    ensureNotNull (nameof array) array
    
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

/// <summary>
/// Returns the concatenation of the array produced by applying the mapping function to each element and its index.
/// </summary>
/// <param name="mapping">The mapping function to apply.</param>
/// <param name="array">The input array.</param>
/// <returns>The concatenation of the array produced by applying the mapping function to each element and its index.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let collecti (mapping: int -> 't -> 'u array) (array: 't array) =
    ensureNotNull (nameof array) array
    
    [|
        let mutable index = 0
        
        for item in array do
            yield! mapping index item
            index <- index + 1
    |]

/// <summary>
/// Filters the array, returning only those elements for which the predicate returned <c>true</c> when applied to said element and its index.
/// </summary>
/// <param name="predicate">The predicate to check pairings of elements and indexes with.</param>
/// <param name="array">The input array.</param>
/// <returns>Those elements for which the predicate returned <c>true</c> when applied to said element and its index.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let filteri predicate (array: 't array) =
    ensureNotNull (nameof array) array
    
    [|
        let mutable index = 0
        
        for item in array do
            if predicate index item then
                yield item

            index <- index + 1
    |]
/// <summary>
/// Filters the array, returning only those elements for which the predicate returned <c>Some(...)</c> when applied to said element and its index.
/// </summary>
/// <param name="predicate">The predicate to check pairings of elements and indexes with.</param>
/// <param name="array">The input array.</param>
/// <returns>Those elements for which the predicate returned <c>Some(...)</c> when applied to said element and its index.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let choosei predicate array =
    ensureNotNull (nameof array) array
    
    [|
        let mutable index = 0
        
        for item in array do
            match predicate index item with
            | Some value -> yield value
            | None -> ()
            
            index <- index + 1
    |]

/// <summary>
/// Removes all instances of the specified value from the array.
/// </summary>
/// <param name="value">The value to remove.</param>
/// <param name="array">The input array.</param>
/// <returns>The input array minus all instances of the given item.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let without (value: 't) (array: 't[]) =
    ensureNotNull (nameof array) array
    
    array
    |> Array.filter ((<>) value)
/// <summary>
/// Removes all instances of the given values from the array.
/// </summary>
/// <param name="values">The values to remove.</param>
/// <param name="array">The input array.</param>
/// <returns>The input array minus all instances of the given items.</returns>
/// <exception cref="System.NullArgumentException"><c>values</c> was null.</exception>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let withoutMany (values: #seq<'t>) (array: 't[]) =
    ensureNotNull (nameof array) array
    ensureNotNull (nameof values) values
    
    let items = Set.ofSeq values

    array
    |> Array.filter (applyBack Set.contains items >> not)

/// <summary>
/// Generates all possible insertions of a value into an array.
/// </summary>
/// <param name="value">The value to insert.</param>
/// <param name="array">The array to insert into.</param>
/// <returns>An array of arrays containing all possible insertions of a value into an array, where each subarray is the result of one possible insertion point.</returns>
/// <example>
/// Executing <c>insertions 4 [ 1; 2; 3 ]</c> would produce:
/// <code>
/// [|
///     [| 4; 1; 2; 3 |]
///     [| 1; 4; 2; 3 |]
///     [| 1; 2; 4; 3 |]
///     [| 1; 2; 3; 4 |]
/// |]
/// </code>
/// </example>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let insertions value array =
    ensureNotNull (nameof array) array
    
    let size = Array.length array + 1
    
    Array.init size (fun insertIndex ->
        Array.init size (fun index ->
            if index = insertIndex then
                value
            else
                let offsetIndex = if index < insertIndex then index else index - 1
                array[offsetIndex]
        )
    )

/// <summary>
/// Computes all possible permutations of the given values.
/// </summary>
/// <param name="array">The input array.</param>
/// <returns>All possible permutations of the given values.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let rec permutations array =
    ensureNotNull (nameof array) array
    
    if Array.isEmpty array then
        [| Array.empty |]
    else
        let head = array[0]
        let tail = array[1..]

        Array.collect (insertions head) (permutations tail)

/// <summary>
/// Returns all pairs where both items are from unique indexes.
/// </summary>
/// <param name="array">The input array.</param>
/// <returns>All pairs where both items are from unique indexes.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let pairs array =
    ensureNotNull (nameof array) array
    
    [|
        for i in 0..(Array.length array - 1) do
            for j in 0..(Array.length array - 1) do
                if i <> j then array[i], array[j]
    |]

/// <summary>
/// Splits the input array into multiple arrays.
/// The provided `options` parameter determines what is done with the separating element.
/// </summary>
/// <param name="options">What to do with the separator.</param>
/// <param name="predicate">The predicate to determine if an element is a separator.</param>
/// <param name="array">The array to split.</param>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let splitByOptions options predicate (array: 't[]) =
    ensureNotNull (nameof array) array
    
    [
        let buffer = ResizeArray()

        for item in array do
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
/// <summary>
/// Splits the input array into multiple arrays.
/// The separating element is not included as part of the results.
/// </summary>
/// <param name="predicate">The predicate to determine if an element is a separator.</param>
/// <param name="array">The array to split.</param>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let splitBy predicate (array: 't[]) = splitByOptions DoNotIncludeSeparator predicate array

/// <summary>
/// Collapses an array according to the specified rule.
/// All pairs which the rule returns <c>Some(x)</c> are replaced by <c>x</c>.
/// This happens recursively for any new pairs created by the merging of a previous pair.
/// </summary>
/// <param name="rule">The rule to apply to each pair of elements.</param>
/// <param name="array">The input array.</param>
/// <returns>The resulting array.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let collapse (rule: 't -> 't -> 't option) array =
    ensureNotNull (nameof array) array
    
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

/// <summary>
/// Determines the number of elements for which the predicate returns true.
/// </summary>
/// <param name="predicate">The predicate to check each item with.</param>
/// <param name="array">The input array.</param>
/// <returns>The number of elements for which the predicate returns true.</returns>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
let count predicate (array: 't[]) =
    ensureNotNull (nameof array) array
    
    let mutable count = 0
    for item in array do if predicate item then count <- count + 1
    count
[<RequireQualifiedAccess>]
module Functional.List

open System.Collections
open System.Collections.Generic
open Functional.Errors.CollectionErrors.PredicationOnItems

// Specific

/// <summary>
/// Adds the item to the beginning of the list.
/// An alias for (::).
/// </summary>
/// <param name="value">The value to prepend.</param>
/// <param name="list">The input list.</param>
/// <returns>The resulting list.</returns>
let inline cons (value: 't) (list: 't list) = value :: list

/// <summary>
/// Returns all the non-empty direct and indirect tails of a list.
/// </summary>
/// <param name="list">The input list.</param>
/// <returns>All the non-empty direct and indirect tails of the list.</returns>
/// <example>
/// Executing <c>tails [ 1; 2; 3; 4 ]</c> results in:
/// <code>
/// [
///     [ 1; 2; 3; 4 ]
///     [ 2; 3; 4 ]
///     [ 3; 4 ]
///     [ 4 ]
/// ]
/// </code>
/// </example>
let tails (list: 't list) =
    let rec loop tails list =
        match list with
        | [] | [ _ ] -> tails
        | _ :: tail ->
            loop (tail :: tails) tail

    match list with
    | [] -> []
    | _ -> loop [ list ] list

// Generic

/// <summary>
/// Returns a list that repeats the given values <c>count</c> times.
/// </summary>
/// <param name="count">The number of times to repeat the values.</param>
/// <param name="list">The values to repeat.</param>
/// <returns>A list that repeats the given values <c>count</c> times.</returns>
/// <exception cref="System.ArgumentException"><c>count</c> is less than zero.</exception>
let repeat count (list: 't[]) =
    ensureNonNegative (nameof count) count
    ensureNotNull (nameof list) list
    [ for _ in 1..count do yield! list ]

/// <summary>
/// Creates a list of lists with the specified number of inner and outer items and initialized with the given function.
/// </summary>
/// <param name="inner">The number of items in each inner list.</param>
/// <param name="outer">The number of lists in the outer list.</param>
/// <param name="initializer">The initializer function.</param>
/// <exception cref="System.ArgumentException"><c>inner</c> was less than zero.</exception>
/// <exception cref="System.ArgumentException"><c>outer</c> was less than zero.</exception>
let table inner outer initializer =
    ensureNonNegative (nameof inner) inner
    ensureNonNegative (nameof outer) outer
    
    List.init outer (fun _ -> List.init inner initializer)

/// <summary>
/// Applies the mapping function to each element of the list, threading an accumulator through the computation, building a new list containing pairs of the state and the mapped item.
/// </summary>
/// <param name="mapping">The mapping to apply.</param>
/// <param name="state">The initial starting state.</param>
/// <param name="list">The input list.</param>
/// <returns>The resulting list.</returns>
let mapScan mapping state list =
    list
    |> List.scan (fun (state, _) -> mapping state) (state, Unchecked.defaultof<'t>)

/// <summary>
/// Applies the mapping function to each element of the list in reverse order, threading an accumulator through the computation, building a new list containing pairs of the state and the mapped item.
/// </summary>
/// <param name="mapping">The mapping to apply.</param>
/// <param name="state">The initial starting state.</param>
/// <param name="list">The input list.</param>
/// <returns>The resulting list.</returns>
let mapScanBack mapping state list =
    List.scanBack (fun item (state, _) ->
        mapping item state
    ) list (state, Unchecked.defaultof<'t>)

/// <summary>
/// Converts an untyped <c>IEnumerator</c> in a list of objects.
/// </summary>
/// <param name="enumerator">The enumerator to convert.</param>
/// <returns>The resulting list.</returns>
/// <exception cref="System.NullArgumentException"><c>enumerator</c> was null.</exception>
let ofUntypedEnumerator (enumerator: IEnumerator) =
    ensureNotNull (nameof enumerator) enumerator
    [ while enumerator.MoveNext() do yield enumerator.Current ]

/// <summary>
/// Converts an <c>IEnumerator&lt;'t&gt;</c> to a list of 't.
/// </summary>
/// <param name="enumerator">The enumerator to convert.</param>
/// <returns>The resulting list.</returns>
/// <exception cref="System.NullArgumentException"><c>enumerator</c> was null.</exception>
let ofEnumerator (enumerator: IEnumerator<'t>) =
    ensureNotNull (nameof enumerator) enumerator
    [ while enumerator.MoveNext() do yield enumerator.Current ]

/// <summary>
/// Converts an object that acts like a collection (that is, has <c>Count</c> and <c>Item</c> defined) to a list.
/// </summary>
/// <param name="collection">The collection to convert.</param>
/// <returns>The resulting list.</returns>
let inline ofCollection< ^c, ^t when ^c : (member Count: int) and ^c : (member Item: int -> ^t) > (collection: ^c) =
    List.init collection.Count collection.Item

/// <summary>
/// Replaces all instances of <c>before</c> in the list with <c>after</c>.
/// </summary>
/// <param name="before">The value to replace.</param>
/// <param name="after">The value to replace with.</param>
/// <param name="list">The input list.</param>
/// <returns>The resulting list.</returns>
let replace before after list =
    list
    |> List.map (fun value -> if value = before then after else value)

/// <summary>
/// Applies a function to each element and its index, threading an accumulator through the computation.
/// </summary>
/// <param name="folder">The function to compute each state given the last.</param>
/// <param name="state">The initial starting state.</param>
/// <param name="list">The input list.</param>
/// <returns>The resulting state.</returns>
let foldi folder (state: 'state) (list: 't list) =    
    let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(folder)
    let rec go list index result =
        match list with
        | [] -> result
        | x :: xs ->
            go xs (index + 1) (f.Invoke(index, result, x))

    go list 0 state

/// <summary>
/// Applies a function to each element and its index in reverse order, threading an accumulator through the computation.
/// </summary>
/// <param name="folder">The function to compute each state given the last.</param>
/// <param name="state">The initial starting state.</param>
/// <param name="list">The input list.</param>
/// <returns>The resulting state.</returns>
let foldBacki folder state list =
    Array.foldBacki folder state (List.toArray list)

/// <summary>
/// Applies the specified folding function to each element as long as the state is not <c>Done</c>.
/// </summary>
/// <param name="folder">The function to generate each state given the previous state.</param>
/// <param name="state">The initial state.</param>
/// <param name="list">The input list.</param>
/// <returns>The resulting state.</returns>
let foldWhile folder state list =
    let rec loop (state: 'state) list =
        match list with
        | [] -> state
        | item :: list ->
            let status, newState = folder state item

            match status with
            | Done -> newState
            | Continue ->
                loop newState list

    loop state list

/// <summary>
/// Applies the specified folding function to each element in reverse order as long as the state is not <c>Done</c>.
/// </summary>
/// <param name="folder">The function to generate each state given the previous state.</param>
/// <param name="state">The initial state.</param>
/// <param name="list">The input list.</param>
/// <returns>The resulting state.</returns>
let foldBackWhile folder list state =
    Array.foldBackWhile folder (List.toArray list) state

/// <summary>
/// Performs a standard fold unless the folding function returns <c>None</c>, in which case the overall function returns <c>None</c>.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="list">The input list.</param>
/// <returns>The resulting state.</returns>
let tryFold folder state list =
    let rec loop (state: 'state) list =
        match list with
        | [] -> Some state
        | item :: list ->
            let newState = folder state item

            match newState with
            | Some state ->
                loop state list
            | None -> None

    loop state list

/// <summary>
/// Performs a standard foldBack unless the folding function returns <c>None</c>, in which case the overall function returns <c>None</c>.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="list">The input list.</param>
/// <returns>The resulting state.</returns>
let tryFoldBack folder list state =
    list
    |> List.toArray
    |> Array.tryFoldBack folder <| state

/// <summary>
/// Combines fold and filter into a single function that threads the state object through the filtering process.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="list">The input list.</param>
/// <returns>The resulting state.</returns>
let thresh folder state list =
    let mutable state = state

    let filtered =
        list
        |> List.filter (fun item ->
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
/// <param name="list">The input list.</param>
/// <returns>The resulting state.</returns>
let threshBack folder list state =
    List.foldBack (fun item (result, state) ->
        let keep, state = folder item state
    
        if keep then
            (item :: result, state)
        else
            (result, state)
    ) list ([], state)

/// <summary>
/// Combines fold and choose into a single function that threads the state object through the filtering process.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="list">The input list.</param>
/// <returns>The resulting state.</returns>
let winnow folder state list =
    let mutable state = state

    [
        for item in list do
            let result, newState = folder state item
            state <- newState
            
            match result with
            | Some x -> yield x
            | None -> ()
    ], state

/// <summary>
/// Combines fold and choose into a single function that threads the state object through the filtering process.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="list">The input list.</param>
/// <returns>The resulting state.</returns>
let winnowBack folder list state =
    List.foldBack (fun item (result, state) ->
        let value, state = folder item state
    
        match value with
        | Some newValue ->
            (newValue :: result), state
        | None ->
            result, state
    ) list ([], state)

/// <summary>
/// Returns the first element for which the given predicate returns <c>true</c>.
/// </summary>
/// <param name="predicate">The predicate to evaluate each item with.</param>
/// <param name="list">The input list.</param>
/// <returns>The first element for which the given predicate returns "true".</returns>
/// <exception cref="Functional.NoSuchItemException">The predicate did not evaluate to true for any items.</exception>
let findi predicate list =
    let rec find index list =
        match list with
        | [] -> noMatchingItem()
        | item :: rest ->
            if predicate index item then
                item
            else
                find (index + 1) rest

    find 0 list

/// <summary>
/// Returns the last element for which the given predicate returns <c>true</c>.
/// </summary>
/// <param name="predicate">The predicate to evaluate each item with.</param>
/// <param name="list">The input list.</param>
/// <returns>The first element for which the given predicate returns <c>true</c>.</returns>
/// <exception cref="Functional.NoSuchItemException">The predicate did not evaluate to true for any items.</exception>
let findBacki predicate list =
    list
    |> List.toArray
    |> Array.findBacki predicate

/// <summary>
/// Returns the first element for which the given predicate returns <c>true</c> or <c>None</c> if there is no such element.
/// </summary>
/// <param name="predicate">The predicate to check each item with.</param>
/// <param name="list">The input list.</param>
/// <returns>The first element for which the given predicate returns <c>true</c> or <c>None</c> if there is no such element.</returns>
let tryFindi predicate list =
    let rec go index list =
        match list with
        | [] -> None
        | item :: rest ->
            if predicate index item then
                Some item
            else
                go (index + 1) rest

    go 0 list

/// <summary>
/// Returns the last element for which the given predicate returns <c>true</c> or <c>None</c> if there is no such element.
/// </summary>
/// <param name="predicate">The predicate to check each item with.</param>
/// <param name="list">The input list.</param>
/// <returns>The last element for which the given predicate returns <c>true</c> or <c>None</c> if there is no such element.</returns>
let tryFindBacki predicate list =
    list
    |> List.toArray
    |> Array.tryFindBacki predicate

/// <summary>
/// Returns the first element for which the given predicate returns <c>Some(x)</c>.
/// </summary>
/// <param name="chooser">The choice function to apply to each element.</param>
/// <param name="list">The input list.</param>
/// <exception cref="Functional.NoSuchItemException">The choice function returned <c>None</c> for all items.</exception>
/// <returns>The first element for which the given predicate returns <c>Some(x)</c>.</returns>
let picki chooser list =
    let rec pick index list =
        match list with
        | [] -> noMatchingItem()
        | item :: rest ->
            match chooser index item with
            | Some v -> v
            | None ->
                pick (index + 1) rest

    pick 0 list

/// Returns the first element for which the given predicate returns Some x.
/// If there is no such element then None is returned instead.
/// <param name="list">The input list.</param>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
/// <returns>The first element for which the given predicate returns <c>Some(x)</c> or <c>None</c> if there is no such item.</returns>
let tryPicki predicate list =
    let rec pick index list =
        match list with
        | [] -> None
        | item :: rest ->
            match predicate index item with
            | Some v -> Some v
            | None ->
                pick (index + 1) rest

    pick 0 list

/// <summary>
/// Returns the concatenation of the arrays produced by applying the mapping function to each element and its index.
/// </summary>
/// <param name="mapping">The mapping function to apply.</param>
/// <param name="list">The input list.</param>
/// <returns>The concatenation of the lists produced by applying the mapping function to each element and its index.</returns>
let collecti mapping list =
    [
        let mutable index = 0
        
        for item in list do
            yield! mapping index item
            index <- index + 1
    ]

/// <summary>
/// Returns only those elements for which the predicate returned <c>true</c> when applied to said element and its index.
/// </summary>
/// <param name="predicate">The predicate to check pairings of elements and indexes with.</param>
/// <param name="list">The input list.</param>
/// <returns>Those elements for which the predicate returned <c>true</c> when applied to said element and its index.</returns>
let filteri predicate list =
    [
        let mutable index = 0
        
        for item in list do
            if predicate index item then yield item
            index <- index + 1
    ]
/// <summary>
/// Returns only those elements for which the predicate returned <c>Some(...)</c> when applied to said element and its index.
/// </summary>
/// <param name="predicate">The predicate to check pairings of elements and indexes with.</param>
/// <param name="list">The input list.</param>
/// <returns>Those elements for which the predicate returned <c>Some(...)</c> when applied to said element and its index.</returns>
let choosei predicate list =
    [
        let mutable index = 0
        
        for item in list do
            match predicate index item with
            | Some value -> yield value
            | None -> ()
            
            index <- index + 1
    ]

/// <summary>
/// Removes all instances of the specified value from the list.
/// </summary>
/// <param name="value">The value to remove.</param>
/// <param name="list">The input list.</param>
/// <returns>The input list minus all instances of the given item.</returns>
let without (value: 't) (list: 't list) =
    list
    |> List.filter ((<>) value)
/// <summary>
/// Removes all instances of the given values from the list.
/// </summary>
/// <param name="values">The values to remove.</param>
/// <param name="list">The input list.</param>
/// <returns>The input list minus all instances of the given items.</returns>
/// <exception cref="System.NullArgumentException"><c>values</c> was null.</exception>
let withoutMany (values: #seq<'t>) (list: 't list) =
    ensureNotNull (nameof values) values
    
    let items = Set.ofSeq values

    list
    |> List.filter (applyBack Set.contains items >> not)

/// <summary>
/// Generates all possible insertions of a value into a list.
/// </summary>
/// <param name="value">The value to insert.</param>
/// <param name="list">The list to insert into.</param>
/// <returns>A list of lists containing all possible insertions of a value into an list, where each sublist is the result of one possible insertion point.</returns>
/// <example>
/// Executing <c>insertions 4 [ 1; 2; 3 ]</c> would produce:
/// <code>
/// [|
///     [ 4; 1; 2; 3 ]
///     [ 1; 4; 2; 3 ]
///     [ 1; 2; 4; 3 ]
///     [ 1; 2; 3; 4 ]
/// |]
/// </code>
/// </example>
let rec insertions value list =
    match list with
    | [] -> [ [ value ] ]
    | y :: ys as xs ->
        (value::xs) :: (List.map (cons y) (insertions value ys))
/// <summary>
/// Computes all possible permutations of the given values.
/// </summary>
/// <param name="list">The input list.</param>
/// <returns>All possible permutations of the given values.</returns>
let rec permutations list =
    match list with
    | [] -> [ [] ]
    | x :: xs ->
        List.collect (insertions x) (permutations xs)

/// <summary>
/// Returns all pairs where both items are from unique indexes.
/// </summary>
/// <param name="list">The input list.</param>
/// <returns>All pairs where both items are from unique indexes.</returns>
let pairs list =
    [
        let mutable outerIndex = 0
        
        for first in list do
            let mutable innerIndex = 0
            
            for second in list do
                if innerIndex <> outerIndex then
                    yield (first, second)
                
                innerIndex <- innerIndex + 1
            
            outerIndex <- outerIndex + 1
    ]

/// <summary>
/// Splits the input list into multiple lists.
/// The provided `options` parameter determines what is done with the separating element.
/// </summary>
/// <param name="options">What to do with the separator.</param>
/// <param name="predicate">The predicate to determine if an element is a separator.</param>
/// <param name="list">The list to split.</param>
let splitByOptions options predicate (list: 't list) =
    [
        let buffer = ResizeArray()

        for item in list do
            if predicate item then
                match options with
                | DoNotIncludeSeparator ->
                    yield buffer.ToArray() |> List.ofArray
                    buffer.Clear()
                | IncludeSeparatorAsFirstElement ->
                    yield buffer.ToArray() |> List.ofArray
                    buffer.Clear()
                    buffer.Add item
                | IncludeSeparatorAsLastElement ->
                    buffer.Add item
                    yield buffer.ToArray() |> List.ofArray
                    buffer.Clear()
                | IncludeSeparatorAsOwnGroup ->
                    yield buffer.ToArray() |> List.ofArray
                    yield List.singleton item
                    buffer.Clear()
            else
                buffer.Add item

        if buffer.Count > 0 then 
            yield buffer.ToArray() |> List.ofArray
    ]
/// <summary>
/// Splits the input list into multiple lists.
/// The separating element is not included as part of the results.
/// </summary>
/// <param name="predicate">The predicate to determine if an element is a separator.</param>
/// <param name="list">The list to split.</param>
let splitBy predicate (list: 't list) = splitByOptions DoNotIncludeSeparator predicate list

/// <summary>
/// Collapses a list according to the specified rule.
/// All pairs which the rule returns <c>Some(x)</c> are replaced by <c>x</c>.
/// This happens recursively for any new pairs created by the merging of a previous pair.
/// </summary>
/// <param name="rule">The rule to apply to each pair of elements.</param>
/// <param name="list">The input list.</param>
/// <returns>The resulting list.</returns>
let collapse (rule: 't -> 't -> 't option) list =
    if List.isEmpty list then
        List.empty
    else
        [
            let mutable state = list.Head
            let mutable list = list.Tail
            
            while not list.IsEmpty do
                match rule state list.Head with
                | Some nextState ->
                    state <- nextState
                    list <- list.Tail
                | None ->
                    yield state
                    state <- list.Head
                    list <- list.Tail
            
            yield state
        ]

/// <summary>
/// Determines the number of elements for which the predicate returns true.
/// </summary>
/// <param name="predicate">The predicate to check each item with.</param>
/// <param name="list">The input list.</param>
/// <returns>The number of elements for which the predicate returns true.</returns>
let count predicate (list: 't list) =
    let mutable count = 0
    
    for item in list do
        if predicate item then
            count <- count + 1
    
    count
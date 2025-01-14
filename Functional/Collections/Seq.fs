[<RequireQualifiedAccess>]
module Functional.Seq

open System.Collections
open System.Collections.Generic
open Functional.Exceptions

// Specific

/// <summary>
/// Creates a sequence by replicating the specified value infinitely.
/// </summary>
/// <param name="element">The element to repeat.</param>
/// <returns>The resulting sequence.</returns>
let replicateInfinite element =
    seq { while true do yield element }
/// <summary>
/// Results in P0, P1, PLast, P0, P1, ....
/// For example <c>Seq.repeatInfinite([ 1; 2; 3 ])</c> produces <c>seq { 1; 2; 3; 1; 2; 3; ... }</c>.
/// </summary>
/// <param name="elements">The elements to repeat.</param>
/// <returns>The resulting sequence.</returns>
let repeatInfinite (elements: #seq<'t>) =
    seq { while true do yield! elements }

// Generic

/// <summary>
/// Repeats the given values <c>count</c> times.
/// </summary>
/// <param name="count">The number of times to repeat the values.</param>
/// <param name="elements">The values to repeat.</param>
/// <returns>A sequence that repeats the given values <c>count</c> times.</returns>
/// <exception cref="System.ArgumentException"><c>count</c> is less than zero.</exception>
let repeat count (elements: #seq<'t>) =
    ensureNonNegative (nameof count) count
    seq { for _ in 1..count do yield! elements }

/// <summary>
/// Creates a table with the specified number of inner and outer items and initialized with the given function.
/// </summary>
/// <param name="inner">The number of items in each row.</param>
/// <param name="outer">The number of rows.</param>
/// <param name="initializer">The initializer function.</param>
/// <exception cref="System.ArgumentException"><c>inner</c> was less than zero.</exception>
/// <exception cref="System.ArgumentException"><c>outer</c> was less than zero.</exception>
/// <returns>The resulting sequence of sequences.</returns>
let table inner outer initializer =
    ensureNonNegative (nameof inner) inner
    ensureNonNegative (nameof outer) outer
    
    Seq.init outer (fun row -> Seq.init inner (initializer row))

/// <summary>
/// Applies the mapping function to each element, threading an accumulator through the computation, returning pairs of the state and the mapped items.
/// </summary>
/// <param name="mapping">The mapping to apply.</param>
/// <param name="state">The initial starting state.</param>
/// <param name="items">The input sequence.</param>
/// <returns>The resulting sequence.</returns>
/// <exception cref="System.NullArgumentException"><c>items</c> was null.</exception>
let mapScan mapping (state : 'state) (items: #seq<'t>) =
    ensureNotNull (nameof items) items
    
    items
    |> Seq.scan (fun (state, _) -> mapping state) (state, Unchecked.defaultof<'t>)
/// <summary>
/// Applies the mapping function to each element in reverse order, threading an accumulator through the computation, returning pairs of the state and the mapped items.
/// </summary>
/// <param name="mapping">The mapping to apply.</param>
/// <param name="state">The initial starting state.</param>
/// <param name="items">The input sequence.</param>
/// <returns>The resulting sequence.</returns>
/// <exception cref="System.NullArgumentException"><c>items</c> was null.</exception>
let mapScanBack mapping (state: 'state) (items: #seq<'t>) =
    ensureNotNull (nameof items) items
    Seq.scanBack (fun item (state, _) -> mapping item state) items (state, Unchecked.defaultof<'t>)

/// <summary>
/// Converts an untyped <c>IEnumerator</c> in sequence of objects.
/// </summary>
/// <param name="enumerator">The enumerator to convert.</param>
/// <returns>The resulting sequence.</returns>
/// <exception cref="System.NullArgumentException"><c>enumerator</c> was null.</exception>
let ofUntypedEnumerator (enumerator: IEnumerator) =
    ensureNotNull (nameof enumerator) enumerator
    
    Seq.unfold (fun () ->
        if enumerator.MoveNext() then
            Some (enumerator.Current, ())
        else
            None
    ) ()
/// <summary>
/// Converts an <c>IEnumerator&lt;'t&gt;</c> to a sequence of 't.
/// </summary>
/// <param name="enumerator">The enumerator to convert.</param>
/// <returns>The resulting sequence.</returns>
/// /// <exception cref="System.NullArgumentException"><c>enumerator</c> was null.</exception>
let ofEnumerator (enumerator: IEnumerator<'t>) =
    Seq.unfold (fun () ->
        if enumerator.MoveNext() then
            Some (enumerator.Current, ())
        else
            None
    ) ()

/// <summary>
/// Converts an object that acts like a collection (that is, has <c>Count</c> and <c>Item</c> defined) to a sequence.
/// </summary>
/// <param name="collection">The collection to convert.</param>
/// <returns>The resulting sequence.</returns>
let inline ofCollection< ^c, ^t when ^c : (member Count: int) and ^c : (member Item: int -> ^t) > (collection: ^c) =
    Seq.init collection.Count collection.Item

/// <summary>
/// Replaces all instances of <c>before</c> in the sequence with <c>after</c>.
/// </summary>
/// <param name="before">The value to replace.</param>
/// <param name="after">The value to replace with.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The resulting sequence.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let replace before after sequence =
    ensureNotNull (nameof sequence) sequence
    
    sequence
    |> Seq.map (fun value -> if value = before then after else value)

/// <summary>
/// Applies a function to each element and its index, threading an accumulator through the computation.
/// </summary>
/// <param name="folder">The function to compute each state given the last.</param>
/// <param name="state">The initial starting state.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let foldi folder (state: 'state) (sequence: #seq<'t>) =    
    ensureNotNull (nameof sequence) sequence

    let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(folder)
    let mutable result = state

    for index, item in Seq.indexed sequence do
        result <- f.Invoke(index, result, item)

    result
/// <summary>
/// Applies a function to each element and its index in reverse order, threading an accumulator through the computation.
/// </summary>
/// <param name="folder">The function to compute each state given the last.</param>
/// <param name="state">The initial starting state.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let foldBacki folder (state: 'state) (sequence: #seq<'t>) =    
    ensureNotNull (nameof sequence) sequence
    Array.foldBacki folder state (Seq.toArray sequence)

/// <summary>
/// Applies the specified folding function to each element as long as the state is not <c>Done</c>.
/// </summary>
/// <param name="folder">The function to generate each state given the previous state.</param>
/// <param name="state">The initial state.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let foldWhile folder (state: 'state) (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    
    use enumerator = sequence.GetEnumerator()
    
    let rec loop state =
        if enumerator.MoveNext() then
            let status, newState = folder state enumerator.Current

            match status with
            | Done -> newState
            | Continue ->
                loop newState
        else
            state

    loop state
/// <summary>
/// Applies the specified folding function to each element in reverse order as long as the state is not <c>Done</c>.
/// </summary>
/// <param name="folder">The function to generate each state given the previous state.</param>
/// <param name="state">The initial state.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let foldBackWhile folder (sequence: #seq<'t>) (state: 'state) =
    ensureNotNull (nameof sequence) sequence
    Array.foldBackWhile folder (Seq.toArray sequence) state

/// <summary>
/// Performs a standard fold unless the folding function returns <c>None</c>, in which case the overall function returns <c>None</c>.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let tryFold folder (state: 'state) (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    
    let enumerator = sequence.GetEnumerator()
    
    let rec loop state =
        if enumerator.MoveNext() then
            let newState = folder state enumerator.Current

            match newState with
            | Some state -> loop state
            | None -> None
        else
            Some state

    loop state
/// <summary>
/// Performs a standard foldBack unless the folding function returns <c>None</c>, in which case the overall function returns <c>None</c>.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let tryFoldBack folder (sequence: #seq<'t>) (state: 'state) =
    ensureNotNull (nameof sequence) sequence
    Array.tryFoldBack folder (Seq.toArray sequence) state

/// <summary>
/// Combines fold and filter into a single function that threads the state object through the filtering process.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let thresh folder (state: 'state) (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    
    let mutable state = state

    let filtered =
        sequence
        |> Seq.filter (fun item ->
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
/// <param name="sequence">The input sequence.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let threshBack folder (sequence: #seq<'t>) (state: 'state) =
    ensureNotNull (nameof sequence) sequence
    
    let array, state = Array.threshBack folder (Seq.toArray sequence) state
    Seq.ofArray array, state

/// <summary>
/// Combines fold and choose into a single function that threads the state object through the filtering process.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let winnow folder (state: 'state) (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    
    let mutable state = state

    let filtered =
        sequence
        |> Seq.choose (fun item ->
            let result, newState = folder state item

            state <- newState
            result
        )

    filtered, state
/// <summary>
/// Combines fold and choose into a single function that threads the state object through the filtering process.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The resulting state.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let winnowBack folder (sequence: #seq<'t>) (state: 'state) =
    ensureNotNull (nameof sequence) sequence
    let array, state = Array.winnowBack folder (Seq.toArray sequence) state
    Seq.ofArray array, state

/// <summary>
/// Returns the first element for which the given predicate returns <c>true</c>.
/// </summary>
/// <param name="predicate">The predicate to evaluate each item with.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The first element for which the given predicate returns <c>true</c>.</returns>
/// <exception cref="Functional.NoSuchItemException">The predicate did not evaluate to true for any items.</exception>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let findi predicate (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    sequence
    |> Seq.indexed
    |> Seq.find (fun (index, item) ->
        predicate index item
    )
    |> snd
/// <summary>
/// Returns the last element for which the given predicate returns <c>true</c>.
/// </summary>
/// <param name="predicate">The predicate to evaluate each item with.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The first element for which the given predicate returns <c>true</c>.</returns>
/// <exception cref="Functional.NoSuchItemException">The predicate did not evaluate to true for any items.</exception>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let findBacki predicate (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    sequence
    |> Seq.indexed
    |> Seq.findBack (fun (index, item) ->
        predicate index item
    )
    |> snd

/// <summary>
/// Returns the first element for which the given predicate returns <c>true</c> or <c>None</c> if there is no such element.
/// </summary>
/// <param name="predicate">The predicate to check each item with.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The first element for which the given predicate returns <c>true</c> or <c>None</c> if there is no such element.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let tryFindi predicate (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    sequence
    |> Seq.indexed
    |> Seq.tryFind (fun (index, item) ->
        predicate index item
    )
    |> Option.map snd
/// <summary>
/// Returns the last element for which the given predicate returns <c>true</c> or <c>None</c> if there is no such element.
/// </summary>
/// <param name="predicate">The predicate to check each item with.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The last element for which the given predicate returns <c>true</c> or <c>None</c> if there is no such element.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let tryFindBacki predicate (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    sequence
    |> Seq.indexed
    |> Seq.tryFindBack (fun (index, item) ->
        predicate index item
    )
    |> Option.map snd

/// <summary>
/// Returns the first element for which the given predicate returns <c>Some(x)</c>.
/// </summary>
/// <param name="chooser">The choice function to apply to each element.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The first element for which the given predicate returns <c>Some(x)</c>.</returns>
/// <exception cref="Functional.NoSuchItemException">The choice function returned <c>None</c> for all items.</exception>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let picki chooser (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    
    try
        sequence
        |> Seq.indexed
        |> Seq.pick (fun (index, item) ->
            chooser index item
        )
    with
    // Replace the more generic KeyNotFoundException with the more specific NoSuchItemException
    | :? KeyNotFoundException as e ->
        raise (NoSuchItemException ("An element matching the predicate was not found in the collection.", e))

/// Returns the first element for which the given predicate returns Some x.
/// If there is no such element then None is returned instead.
/// <param name="sequence">The input sequence.</param>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
/// <returns>The first element for which the given predicate returns <c>Some(x)</c> or <c>None</c> if there is no such item.</returns>
let tryPicki chooser (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    
    sequence
    |> Seq.indexed
    |> Seq.tryPick (fun (index, item) ->
        chooser index item
    )

/// <summary>
/// Returns the concatenation of the sequences produced by applying the mapping function to each element and its index.
/// </summary>
/// <param name="mapping">The mapping function to apply.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The concatenation of the sequences produced by applying the mapping function to each element and its index.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let collecti mapping (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    sequence
    |> Seq.indexed
    |> Seq.collect (fun (index, item) ->
        mapping index item
    )

/// <summary>
/// Returns only those elements for which the predicate returned <c>true</c> when applied to said element and its index.
/// </summary>
/// <param name="predicate">The predicate to check pairings of elements and indexes with.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>Those elements for which the predicate returned <c>true</c> when applied to said element and its index.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let filteri predicate (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    
    seq {
        let mutable index = 0
        
        for item in sequence do
            if predicate index item then yield item
            index <- index + 1
    }
/// <summary>
/// Returns only those elements for which the predicate returned <c>Some(...)</c> when applied to said element and its index.
/// </summary>
/// <param name="predicate">The predicate to check pairings of elements and indexes with.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>Those elements for which the predicate returned <c>Some(...)</c> when applied to said element and its index.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let choosei predicate (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    
    seq {
        let mutable index = 0

        for item in sequence do
            match predicate index item with
            | Some value -> yield value
            | None -> ()
    }

/// <summary>
/// Removes all instances of the specified value from the sequence.
/// </summary>
/// <param name="value">The value to remove.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The input sequence minus all instances of the given item.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let without (value: 't) (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    sequence |> Seq.filter ((<>) value)
/// <summary>
/// Removes all instances of the given values from the sequence.
/// </summary>
/// <param name="values">The values to remove.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The input sequence minus all instances of the given items.</returns>
/// <exception cref="System.NullArgumentException"><c>values</c> was null.</exception>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let withoutMany (values: #seq<'t>) (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    ensureNotNull (nameof values) values
    
    let items = Set.ofSeq values

    sequence
    |> Seq.filter (Set.contains @@ items >> not)

/// <summary>
/// Generates all possible insertions of a value into an sequence.
/// </summary>
/// <param name="value">The value to insert.</param>
/// <param name="sequence">The sequence to insert into.</param>
/// <returns>An sequence of sequences containing all possible insertions of a value into an sequence, where each subsequence is the result of one possible insertion point.</returns>
/// <example>
/// Executing <c>insertions 4 (seq { 1; 2; 3 })</c> would produce:
/// <code>
/// seq {
///     seq { 4; 1; 2; 3 }
///     seq { 1; 4; 2; 3 }
///     seq { 1; 2; 4; 3 }
///     seq { 1; 2; 3; 4 }
/// }
/// </code>
/// </example>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let insertions value (sequence: #seq<'t>) =
    let items = Seq.toArray sequence
    let size = items.Length + 1
    
    Seq.init size (fun insertIndex ->
        seq {
            let mutable index = 0
            for item in items do
                if index = insertIndex then yield value
                yield item
        }
    )
/// <summary>
/// Computes all possible permutations of the given values.
/// </summary>
/// <param name="sequence">The input sequence.</param>
/// <returns>All possible permutations of the given values.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let permutations (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    
    let rec go (sequence: seq<'t>) =
        match Seq.tryHead sequence with
        | Some head ->
            let tail = Seq.tail sequence
            Seq.collect (insertions head) (go tail)
        | None ->
            seq { Seq.empty }
            
    go sequence

/// <summary>
/// Returns all pairs where both items are from unique indexes.
/// </summary>
/// <param name="sequence">The input sequence.</param>
/// <returns>All pairs where both items are from unique indexes.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let pairs (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    
    seq {
        for i, vi in Seq.indexed sequence do
            for j, vj in Seq.indexed sequence do
                if i <> j then yield (vi, vj)
    }

/// <summary>
/// Splits the input sequence into multiple sequences.
/// The provided `options` parameter determines what is done with the separating element.
/// </summary>
/// <param name="options">What to do with the separator.</param>
/// <param name="predicate">The predicate to determine if an element is a separator.</param>
/// <param name="sequence">The sequence to split.</param>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let splitByOptions options predicate (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    
    [|
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
    |]
/// <summary>
/// Splits the input sequence into multiple sequences.
/// The separating element is not included as part of the results.
/// </summary>
/// <param name="predicate">The predicate to determine if an element is a separator.</param>
/// <param name="sequence">The sequence to split.</param>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let splitBy predicate (sequence: #seq<'t>) = splitByOptions DoNotIncludeSeparator predicate sequence

/// <summary>
/// Collapses an sequence according to the specified rule.
/// All pairs which the rule returns <c>Some(x)</c> are replaced by <c>x</c>.
/// This happens recursively for any new pairs created by the merging of a previous pair.
/// </summary>
/// <param name="rule">The rule to apply to each pair of elements.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The resulting sequence.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let collapse (rule: 't -> 't -> 't option) (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    
    seq {
        let enumerator = sequence.GetEnumerator()
        
        if enumerator.MoveNext() then
            let mutable state = enumerator.Current
            
            while enumerator.MoveNext() do
                match rule state enumerator.Current with
                | Some nextState ->
                    state <- nextState
                | None ->
                    yield state
                    state <- enumerator.Current

            yield state
    }

/// <summary>
/// Determines the number of elements for which the predicate returns true.
/// </summary>
/// <param name="predicate">The predicate to check each item with.</param>
/// <param name="sequence">The input sequence.</param>
/// <returns>The number of elements for which the predicate returns true.</returns>
/// <exception cref="System.NullArgumentException"><c>sequence</c> was null.</exception>
let count predicate (sequence: #seq<'t>) =
    ensureNotNull (nameof sequence) sequence
    
    let mutable count = 0
    
    for item in sequence do
        if predicate item then
            count <- count + 1
    
    count
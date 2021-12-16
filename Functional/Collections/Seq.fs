[<RequireQualifiedAccess>]
module Functional.Seq

open System.Collections
open System.Collections.Generic

/// Creates a sequence of sequences with the specified dimensions initialized with the specified function.
let table inner outer initializer =
    Seq.init outer (fun _ -> Seq.init inner initializer)

/// Combines map and scan.
let mapScan action state (items: #seq<'t>) =
    items
    |> Seq.scan (fun (state, _) -> action state) (state, Unchecked.defaultof<'t>)
/// Combines map and scanBack.
let mapScanBack action state (items: #seq<'t>) =
    Seq.scanBack (fun item (state, _) -> action item state) items (state, Unchecked.defaultof<'t>)

/// Converts an untyped IEnumerator to an object sequence.
let fromUntypedEnumerator (enumerator: IEnumerator) =
    Seq.unfold (fun () ->
        if enumerator.MoveNext() then
            Some (enumerator.Current, ())
        else
            None
    ) ()
/// Converts an IEnumerator<'t> to a 't sequence.
let fromEnumerator (enumerator: IEnumerator<'t>) =
    Seq.unfold (fun () ->
        if enumerator.MoveNext() then
            Some (enumerator.Current, ())
        else
            None
    ) ()

/// Converts a loose collection like XmlNodeList
let inline fromLooseCollection< ^c, ^t when ^c : (member Count: int) and ^c : (member Item: int -> ^t) > (collection: ^c) =
    let count = (^c : (member Count: int) collection)
    
    Seq.init count (fun index ->
        (^c : (member Item: int -> ^t) (collection, index))
    )

/// Acting as a combination of map and choose, the resulting collection contains the elements from the original sequence for which the replacement function returned none.
/// If the replacement function returned Some(x) instead, then the value of x replaces the original element from the collection.
let replace replacement sequence =
    sequence
    |> Seq.map (fun item ->
        replacement item
        |> Option.defaultValue item
    )
/// Creates a new sequence with the value at the specified index replaced with the specified value.
/// If the index is out of range, then the operation does nothing.
let updateAt index value sequence =
    sequence
    |> Seq.mapi (fun current old ->
        if current = index then value else old
    )

/// Applies the specified folding function to the sequence as long as the state is not None.
let foldWhile folder state (sequence: #seq<_>) =
    let enumerator = sequence.GetEnumerator()
    
    let rec loop state =
        if enumerator.MoveNext() then
            let newState = folder state enumerator.Current

            match newState with
            | Some state -> loop state
            | None -> state
        else
            state

    loop state
/// Applies the specified folding function to the sequence as long as the state is not None.
let foldBackWhile folder sequence state =
    Array.foldBackWhile folder (Seq.toArray sequence) state
    |> Seq.ofArray

/// Performs a standard fold unless the folding function returns none, in which case the overall function returns none.
let tryFold folder state (sequence: #seq<_>) =
    let enumerator = sequence.GetEnumerator()
    
    let rec loop state =
        if enumerator.MoveNext() then
            let newState = folder state enumerator.Current

            match newState with
            | Some state -> loop state
            | None -> None
        else
            state

    loop state
/// Performs a standard fold back unless the folding function returns none, in which case the overall function returns none.
let tryFoldBack folder sequence state =
    Array.tryFoldBack folder (Seq.toArray sequence) state

/// Combines fold and filter into a single function that threads the state object through the filtering process.
let thresh folder state (sequence: #seq<_>) =
    let mutable state = state

    let filtered =
        sequence
        |> Seq.filter (fun item ->
            let keep, newState = folder state item
            state <- newState
            keep
        )

    filtered, state
/// Combines foldBack and filter into a single function that threads the state object through the filtering process.
let threshBack folder sequence state =
    let mutable state = state

    let filtered =
        sequence
        |> Seq.rev
        |> Seq.filter (fun item ->
            let keep, newState = folder state item
            state <- newState
            keep
        )

    filtered, state

/// Combines fold and choose into a single function that threads the state object through the filtering process.
let winnow folder state sequence =
    let mutable state = state

    let filtered =
        sequence
        |> Seq.choose (fun item ->
            let result, newState = folder state item

            state <- newState
            result
        )

    filtered, state
/// Combines fold and choose into a single function that threads the state object through the filtering process.
let winnowBack folder sequence state =
    sequence
    |> Seq.rev
    |> winnow (fun a b -> folder b a) state

/// Returns the first element for which the given predicate returns "true".
/// If there is no such element then a KeyNotFoundException is raised instead.
let findi predicate sequence =
    let mutable index = -1
    
    sequence
    |> Seq.find (fun item ->
        index <- index + 1
        predicate index item
    )

/// Returns the last element for which the given predicate returns "true".
/// If there is no such element then a KeyNotFoundException is raised instead.
let findBacki predicate sequence =
    let mutable index = -1
    
    sequence
    |> Seq.findBack (fun item ->
        index <- index + 1
        predicate index item
    )

/// Returns the first element for which the given predicate returns "true".
/// If there is no such element then None is returned instead.
let tryFindi predicate sequence =
    let mutable index = -1
    
    sequence
    |> Seq.tryFind (fun item ->
        index <- index + 1
        predicate index item
    )

/// Returns the last element for which the given predicate returns "true".
/// If there is no such element then None is returned instead.
let tryFindBacki predicate sequence =
    let mutable index = -1
    
    sequence
    |> Seq.tryFindBack (fun item ->
        index <- index + 1
        predicate index item
    )

/// Returns the first element for which the given predicate returns Some x.
/// If there is no such element then a KeyNotFoundException is raised instead.
let picki chooser sequence =
    let mutable index = -1
    
    sequence
    |> Seq.pick (fun item ->
        index <- index + 1
        chooser index item
    )

/// Returns the first element for which the given predicate returns Some x.
/// If there is no such element then None is returned instead.
let tryPicki chooser sequence =
    let mutable index = -1
    
    sequence
    |> Seq.tryPick (fun item ->
        index <- index + 1
        chooser index item
    )

/// For each element apply the given function, concatenate all the results and returned the combined result.
let collecti mapping sequence =
    let mutable index = -1

    sequence
    |> Seq.collect (fun item ->
        index <- index + 1
        mapping index item
    )

/// Returns a new collection containing only the elements of the collection for which the given predicate returns true.
let filteri predicate sequence =
    let mutable index = -1

    sequence
    |> Seq.filter (fun item ->
        index <- index + 1
        predicate index item
    )
/// Returns a new collection containing only the elements of the collection for which the given predicate returns Some;
let choosei predicate sequence =
    let mutable index = -1

    sequence
    |> Seq.choose (fun item ->
        index <- index + 1
        predicate index item
    )

/// Removes all instances of the specified item from the sequence.
let without (item: 't) (sequence: #seq<'t>) =
    sequence
    |> Seq.filter ((<>) item)
/// Removes the item a the specified index (if it is within the sequence).
let removeAt (index: int) (sequence: #seq<'t>) =
    sequence
    |> filteri (fun current _ -> current <> index)

/// Adds the item to the beginning of the collection.
let prependItem (item: 't) (sequence: #seq<'t>) =
    seq {
        yield item
        yield! sequence
    }
/// Adds the item to the end of the collection.
let appendItem (item: 't) (sequence: #seq<'t>) =
    seq {
        yield! sequence
        yield item
    }

/// All possible insertions of a given item into the sequence.
/// insertions 4 [ 1; 2; 3 ] -> [ 4; 1; 2; 3 ]; [ 1; 4; 2; 3 ]; [ 1; 2; 4; 3 ]; [ 1; 2; 3; 4 ]
let insertions x items =
    let items = Seq.toArray items
    let size = items.Length + 1
    
    Seq.init size (fun insertIndex ->
        Seq.init size (fun index ->
            if index = insertIndex then
                x
            else
                let offsetIndex = if index < insertIndex then index else index - 1
                items.[offsetIndex]
        )
    )

/// Results in P0, P1, PLast, P0, P1, ....
/// Example: Seq.cycle([ 1; 2; 3 ]) ==> seq { 1; 2; 3; 1; 2; 3; ... }
let cycle (elements: #seq<'t>) =
    seq {
        while true do
            yield! elements
    }

// Returns all pairs where both items are from unique indexes.
let pairs sequence =
    sequence
    |> collecti (fun outerIndex a ->
        sequence
        |> choosei (fun innerIndex b ->
            if outerIndex = innerIndex then
                None
            else
                Some (a, b)
        )
    )

/// Splits the input sequence based on the specified rule function.
/// The item that is split on is not included in the results.
let splitBy (rule: 't -> bool) (sequence: #seq<'t>) =
    seq {
        let enumerator = sequence.GetEnumerator()
        let buffer = ResizeArray()

        while enumerator.MoveNext() do
            let item = enumerator.Current

            if rule item then
                yield buffer.ToArray() |> Seq.ofArray
                buffer.Clear()
            else
                buffer.Add item

        if buffer.Count > 0 then 
            yield buffer.ToArray() |> Seq.ofArray
    }

/// Splits the input array based on the specified rule function.
/// The item that is split on is included in the results as the first item of each section.
let chunkBy (rule: 't -> bool) (sequence: #seq<'t>) =
    seq {
        let enumerator = sequence.GetEnumerator()
        let buffer = ResizeArray()

        while enumerator.MoveNext() do
            let item = enumerator.Current

            if rule item then
                yield buffer.ToArray() |> Seq.ofArray
                buffer.Clear()
                buffer.Add item
            else
                buffer.Add item

        if buffer.Count > 0 then 
            yield buffer.ToArray() |> Seq.ofArray
    }
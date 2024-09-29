[<RequireQualifiedAccess>]
module Functional.List

open System.Collections
open System.Collections.Generic

// Generic

// Specific

// Unsorted

/// Creates a list of lists with the specified dimensions initialized with the specified function.
let table inner outer initializer =
    List.init outer (fun _ -> List.init inner initializer)

/// Maps the given action over list of lists.
let mapTable action table =
    table
    |> List.map (
        List.map action
    )
    
/// Maps the given action over a table, passing in a tuple of the row and column numbers.
let mapTablei action table =
    table
    |> List.mapi (fun rowNumber row ->
        row
        |> List.map (fun columnNumber ->
            action (rowNumber, columnNumber)
        )
    )

/// Combines map and scan.
let mapScan action state items =
    items
    |> List.scan (fun (state, _) -> action state) (state, Unchecked.defaultof<'t>)

/// Combines map and scanBack.
let mapScanBack action state items =
    List.scanBack (fun item (state, _) ->
        action item state
    ) items (state, Unchecked.defaultof<'t>)

/// Converts an untyped IEnumerator to an object list.
let fromUntypedEnumerator (enumerator: IEnumerator) =
    List.unfold (fun () ->
        if enumerator.MoveNext() then
            Some (enumerator.Current, ())
        else
            None
    ) ()
/// Converts an IEnumerator<'t> to a 't list.
let fromEnumerator (enumerator: IEnumerator<'t>) =
    List.unfold (fun () ->
        if enumerator.MoveNext() then
            Some (enumerator.Current, ())
        else
            None
    ) ()

/// Converts a loose collection like XmlNodeList
let inline fromLooseCollection< ^c, ^t when ^c : (member Count: int) and ^c : (member Item: int -> ^t) > (collection: ^c) =
    let count = (^c : (member Count: int) collection)
    
    List.init count (fun index ->
        (^c : (member Item: int -> ^t) (collection, index))
    )

/// Replaces the specified value 'before' with the value 'after'.
let replace before after list =
    list
    |> List.map (fun value -> if value = before then after else value)

/// Acting as a combination of map and choose, the resulting collection contains the elements from the original list for which the replacement function returned none.
/// If the replacement function returned Some(x) instead, then the value of x replaces the original element from the collection.
let replaceWith replacement list =
    list
    |> List.map (fun item ->
        replacement item
        |> Option.defaultValue item
    )

/// Creates a new list with the value at the specified index replaced with the specified value.
/// If the index is out of range, then the original list is returned.
let updateAt index value list =
    let original = list
    
    let rec update index passed list =
        match list with
        | [] -> original
        | head :: rest ->
            if index = 0 then
                List.append (List.rev passed) (value :: rest)
            else
                update (index - 1) (head :: passed) rest
    
    update index [] list

/// Applies the specified folding function to the list as long as the state is not None.
let foldWhile folder state list =
    let rec loop (state: 'state) list =
        match list with
        | [] -> state
        | item :: list ->
            let newState = folder state item

            match newState with
            | Done value -> value
            | Continue state ->
                loop state list

    loop state list
/// Applies the specified folding function to the list as long as the state is not None.
let foldBackWhile folder list state =
    list
    |> List.toArray
    |> Array.foldBackWhile folder <| state

/// Performs a standard fold unless the folding function returns none, in which case the overall function returns none.
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
/// Performs a standard fold back unless the folding function returns none, in which case the overall function returns none.
let tryFoldBack folder list state =
    list
    |> List.toArray
    |> Array.tryFoldBack folder <| state

/// Combines fold and filter into a single function that threads the state object through the filtering process.
let thresh folder state list =
    let list, state =
        List.fold (fun (result, state) item ->
            let keep, state = folder state item

            if keep then
                (item :: result, state)
            else
                (result, state)
        ) ([], state) list

    (List.rev list), state
/// Combines foldBack and filter into a single function that threads the state object through the filtering process.
let threshBack folder list state =
    List.foldBack (fun item (result, state) ->
        let keep, state = folder item state
    
        if keep then
            (item :: result, state)
        else
            (result, state)
    ) list ([], state)

/// Combines fold and choose into a single function that threads the state object through the filtering process.
let winnow folder state list =
    let list, state =
        List.fold (fun (result, state) item ->
            let value, state = folder state item

            match value with
            | Some newValue ->
                (newValue :: result, state)
            | None ->
                (result, state)
        ) ([], state) list

    (List.rev list), state
/// Combines fold and choose into a single function that threads the state object through the filtering process.
let winnowBack folder list state =
    List.foldBack (fun item (result, state) ->
        let value, state = folder item state
    
        match value with
        | Some newValue ->
            (newValue :: result), state
        | None ->
            result, state
    ) list ([], state)

/// Returns the first element for which the given predicate returns "true".
/// If there is no such element then a KeyNotFoundException is raised instead.
let findi predicate list =
    let rec find index list =
        match list with
        | [] -> raise (KeyNotFoundException "An element matching the predicate was not found in the list.")
        | item :: rest ->
            if predicate index item then
                item
            else
                find (index + 1) rest

    find 0 list

/// Returns the last element for which the given predicate returns "true".
/// If there is no such element then a KeyNotFoundException is raised instead.
let findBacki predicate list =
    list
    |> List.toArray
    |> Array.findBacki predicate

/// Returns the first element for which the given predicate returns "true".
/// If there is no such element then None is returned instead.
let tryFindi predicate list =
    let rec find index list =
        match list with
        | [] -> None
        | item :: rest ->
            if predicate index item then
                Some item
            else
                find (index + 1) rest

    find 0 list

/// Returns the last element for which the given predicate returns "true".
/// If there is no such element then None is returned instead.
let tryFindBacki predicate list =
    list
    |> List.toArray
    |> Array.tryFindBacki predicate

/// Returns the first element for which the given predicate returns Some x.
/// If there is no such element then a KeyNotFoundException is raised instead.
let picki predicate list =
    let rec pick index list =
        match list with
        | [] -> raise (KeyNotFoundException "An element matching the predicate was not found in the list.")
        | item :: rest ->
            match predicate index item with
            | Some v -> v
            | None ->
                pick (index + 1) rest

    pick 0 list

/// Returns the first element for which the given predicate returns Some x.
/// If there is no such element then None is returned instead.
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

/// For each element apply the given function, concatenate all the results and returned the combined result.
let collecti mapping list =
    [
        let mutable index = 0
        
        for item in list do
            yield! mapping index item
            index <- index + 1
    ]

/// Returns a new collection containing only the elements of the collection for which the given predicate returns true.
let filteri predicate list =
    [
        let mutable index = 0
        
        for item in list do
            if predicate index item then yield item
            index <- index + 1
    ]
/// Returns a new collection containing only the elements of the collection for which the given predicate returns Some;
let choosei predicate list =
    [
        let mutable index = 0
        
        for item in list do
            match predicate index item with
            | Some value -> yield value
            | None -> ()
            
            index <- index + 1
    ]

/// Removes all instances of the specified item from the list.
let without (item: 't) (list: 't list) =
    list
    |> List.filter ((<>) item)

/// Adds the item to the beginning of the list.
/// An alias for prepend and (::)
let inline cons (item: 't) (list: 't list) =
    item :: list
/// Adds the item to the beginning of the collection.
let inline prependItem (item: 't) (list: 't list) =
    item :: list
/// Adds the item to the end of the collection.
let appendItem (item: 't) (list: 't list) =
    List.append list [ item ]

/// All possible insertions of a given item into the list.
/// insertions 4 [ 1; 2; 3 ] -> [ 4; 1; 2; 3 ]; [ 1; 4; 2; 3 ]; [ 1; 2; 4; 3 ]; [ 1; 2; 3; 4 ]
let rec insertions x items =
    match items with
    | [] -> [ [ x ] ]
    | y :: ys as xs ->
        (x::xs) :: (List.map (prependItem y) (insertions x ys))
/// All possible permutations.
let rec permutations items =
    match items with
    | [] -> [ [] ]
    | x :: xs ->
        List.collect (insertions x) (permutations xs)

/// Returns all the non-empty direct and indirect tails of a list.
/// Tails [ 1; 2; 3; 4 ] ==> [ [ 1; 2; 3; 4 ]; [ 2; 3; 4 ]; [ 3; 4 ]; [ 4 ] ]
let tails (list: 't list) =
    let rec loop tails list =
        match list with
        | [] | [ _ ] -> tails
        | _ :: tail ->
            loop (tail :: tails) tail

    match list with
    | [] -> []
    | _ -> loop [ list ] list

// Returns all pairs where both items are from unique indexes.
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

/// Splits the input sequence based on the specified rule function.
/// The item that is split on is not included in the results.
let splitBy (rule: 't -> bool) (sequence: 't list) =
    [
        let buffer = ResizeArray()

        for item in sequence do
            if rule item then
                yield buffer.ToArray() |> List.ofArray
                buffer.Clear()
            else
                buffer.Add item

        if buffer.Count > 0 then 
            yield buffer.ToArray() |> List.ofArray
    ]

/// Splits the input sequence based on the specified rule function.
/// The item that is split on is included in the results as the first item of each section.
let chunkByFirst (rule: 't -> bool) (sequence: 't list) =
    [
        let buffer = ResizeArray()

        for item in sequence do

            if rule item then
                yield buffer.ToArray() |> List.ofArray
                buffer.Clear()
                buffer.Add item
            else
                buffer.Add item

        if buffer.Count > 0 then 
            yield buffer.ToArray() |> List.ofArray
    ]

/// Splits the input sequence based on the specified rule function.
/// The item that is split on is included in the results as the last item of each section.
let chunkByLast (rule: 't -> bool) (sequence: 't list) =
    [
        let buffer = ResizeArray()

        for item in sequence do
            if rule item then
                buffer.Add item
                yield buffer.ToArray() |> List.ofArray
                buffer.Clear()
            else
                buffer.Add item

        if buffer.Count > 0 then 
            yield buffer.ToArray() |> List.ofArray
    ]

/// Splits the input sequence based on the specified rule function.
/// The item that is split on is included in the results as its own group.
let separateBy (rule: 't -> bool) (sequence: 't list) =
    let group = ResizeArray()
    
    [
        for item in sequence do
            if rule item then
                yield group.ToArray() |> List.ofArray
                yield [ item ]
                group.Clear()
            else
                group.Add item

        if group.Count > 0 then
            yield group.ToArray() |> List.ofArray
    ]
    
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
        
/// Counts the number of items that match the specified rule.
let count rule (array: 't list) =
    let mutable count = 0
    
    for item in array do
        if rule item then
            count <- count + 1
    
    count
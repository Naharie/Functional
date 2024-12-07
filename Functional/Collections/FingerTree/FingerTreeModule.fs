[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Functional.FingerTree

// So we can use FingerTreeShared without warnings
#nowarn "0044"

open System
open System.Collections
open System.Collections.Generic

open Functional
open Functional.Errors.CollectionErrors
open Functional.FingerTree.Builder

/// <summary>
/// Converts a digit to a finger tree.
/// </summary>
/// <param name="digit">The digit to convert.</param>
/// <returns>The resulting tree.</returns>
let inline private promoteDigit digit =
    match digit with
    | One a -> Single a
    | Two (a, b) -> Deep(One a, Blank, One b)
    | Three (a, b, c) -> Deep (One a, Blank, Two (b, c))
    | Four (a, b, c, d) -> Deep (Two (a, b), Blank, Two (c, d))

let private fingerTree = FingerTreeBuilder()

// Specific

/// <summary>
/// Attempts to deconstruct the tree into the leftmost item and a tree containing all the remaining items.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns><c>ValueSome(head, tail)</c> if the tree is non-empty, or <c>ValueNone</c> if it is.</returns>
let rec viewV<'t> (tree: FingerTree<'t>) : ('t * FingerTree<'t>) ValueOption =
    match tree with
    | Blank -> ValueNone
    | Single value -> ValueSome(value, Blank)
    
    | Deep(One value, middle, right) ->
        let rest =
            match viewV middle with
            | ValueNone -> promoteDigit right
            | ValueSome (node, rest) ->
                Deep (Node.toDigit node, rest, right)
        
        ValueSome(value, rest)
    
    | Deep(Two (a, b), middle, right) ->
        ValueSome(a, Deep (One b, middle, right))
    
    | Deep(Three (a, b, c), middle, right) ->
        ValueSome(a, Deep (Two (b, c), middle, right))
    
    | Deep(Four (a, b, c, d), middle, right) ->
        ValueSome(a, Deep (Three (b, c, d), middle, right))
/// <summary>
/// Attempts to deconstruct the tree into the leftmost item and a tree containing all the remaining items.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns><c>Some(head, tail)</c> if the tree is non-empty, or <c>None</c> if it is.</returns>
let rec view<'t> (tree: FingerTree<'t>) : ('t * FingerTree<'t>) option =
    match tree with
    | Blank -> None
    | Single value -> Some(value, Blank)
    
    | Deep(One value, middle, right) ->
        let rest =
            match viewV middle with
            | ValueNone -> promoteDigit right
            | ValueSome (node, rest) ->
                Deep (Node.toDigit node, rest, right)
        
        Some(value, rest)
    
    | Deep(Two (a, b), middle, right) ->
        Some(a, Deep (One b, middle, right))
    
    | Deep(Three (a, b, c), middle, right) ->
        Some(a, Deep (Two (b, c), middle, right))
    
    | Deep(Four (a, b, c, d), middle, right) ->
        Some(a, Deep (Three (b, c, d), middle, right))

/// <summary>
/// Attempts to deconstruct the tree into the rightmost item and a tree containing all the remaining items.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns><c>Some(rightMost, remainingItems)</c> if the tree is non-empty, or <c>None</c> if it is.</returns>
let rec viewRev<'t> (tree: FingerTree<'t>): ('t * FingerTree<'t>) option =
    match tree with
    | Blank -> None
    | Single value -> Some(value, Blank)
    
    | Deep(left, middle, One value) ->
        let rest =
            match viewV middle with
            | ValueNone -> promoteDigit left
            | ValueSome (node, rest) ->
                Deep (left, rest, Node.toDigit node)
        
        Some(value, rest)
    
    | Deep(left, middle, Two (b, a)) ->
        Some (a, Deep (left, middle, One b))
    
    | Deep(left, middle, Three (c, b, a)) ->
        Some(a, Deep (left, middle, Two (c, b)))
    
    | Deep(left, middle, Four (d, c, b, a)) ->
        Some(a, Deep (left, middle, Three (d, c, b)))
/// <summary>
/// Attempts to deconstruct the tree into the rightmost item and a tree containing all the remaining items.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns><c>ValueSome(rightMost, remainingItems)</c> if the tree is non-empty, or <c>ValueNone</c> if it is.</returns>
let rec viewRevV<'t> (tree: FingerTree<'t>): ('t * FingerTree<'t>) ValueOption =
    match tree with
    | Blank -> ValueNone
    | Single value -> ValueSome(value, Blank)
    
    | Deep(left, middle, One value) ->
        let rest =
            match viewV middle with
            | ValueNone -> promoteDigit left
            | ValueSome (node, rest) ->
                Deep (left, rest, Node.toDigit node)
        
        ValueSome(value, rest)
    
    | Deep(left, middle, Two (b, a)) ->
        ValueSome (a, Deep (left, middle, One b))
    
    | Deep(left, middle, Three (c, b, a)) ->
        ValueSome(a, Deep (left, middle, Two (c, b)))
    
    | Deep(left, middle, Four (d, c, b, a)) ->
        ValueSome(a, Deep (left, middle, Three (d, c, b)))

/// <summary>
/// Attempts to return the leftmost item of the tree.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The leftmost item or <c>None</c> if the tree is empty.</returns>
let left (tree: FingerTree<'t>) =
    match tree with
    | Blank -> None
    | Single x -> Some x
    | Deep (One x, _, _) -> Some x
    | Deep (Two (x, _), _, _) -> Some x
    | Deep (Three (x, _, _), _, _) -> Some x
    | Deep (Four (x, _, _, _), _, _) -> Some x
/// <summary>
/// Attempts to return the leftmost item of the tree.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The leftmost item or <c>ValueNone</c> if the tree is empty.</returns>
let leftV (tree: FingerTree<'t>) =
    match tree with
    | Blank -> ValueNone
    | Single x -> ValueSome x
    | Deep (One x, _, _) -> ValueSome x
    | Deep (Two (x, _), _, _) -> ValueSome x
    | Deep (Three (x, _, _), _, _) -> ValueSome x
    | Deep (Four (x, _, _, _), _, _) -> ValueSome x

/// <summary>
/// Attempts to return the rightmost item of the tree.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The rightmost item or <c>None</c> if the tree is empty.</returns>
let right (tree: FingerTree<'t>) : 't option =
    match tree with
    | Blank -> None
    | Single x -> Some x
    | Deep (_, _, One x) -> Some x
    | Deep (_, _, Two (_, x)) -> Some x
    | Deep (_, _, Three (_, _, x)) -> Some x
    | Deep (_, _, Four (_, _, _, x)) -> Some x
/// <summary>
/// Attempts to return the rightmost item of the tree.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The rightmost item or <c>ValueNone</c> if the tree is empty.</returns>
let rightV tree =
    match tree with
    | Blank -> ValueNone
    | Single x -> ValueSome x
    | Deep (_, _, One x) -> ValueSome x
    | Deep (_, _, Two (_, x)) -> ValueSome x
    | Deep (_, _, Three (_, _, x)) -> ValueSome x
    | Deep (_, _, Four (_, _, _, x)) -> ValueSome x

/// <summary>
/// Returns the input tree with all but the leftmost item.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The input tree with all but the leftmost item.</returns>
let rec removeLeft<'t> (tree: FingerTree<'t>): FingerTree<'t> =
    match tree with
    | Blank -> Blank
    | Single _ -> Blank
    
    | Deep(One _, middle, right) ->
        match viewV middle with
        | ValueNone -> promoteDigit right
        | ValueSome (node, rest) ->
            Deep (Node.toDigit node, rest, right)

    | Deep(Two (_, b), middle, right) ->
        Deep (One b, middle, right)
    
    | Deep(Three (_, b, c), middle, right) ->
        Deep (Two (b, c), middle, right)
    
    | Deep(Four (_, b, c, d), middle, right) ->
        Deep (Three (b, c, d), middle, right)
/// <summary>
/// Returns the input tree with all but the rightmost item.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The input tree with all but the rightmost item.</returns>
let removeRight (tree: FingerTree<'t>) =
    match tree with
    | Blank -> Blank
    | Single _ -> Blank
    
    | Deep(left, middle, One _) ->
        match viewV middle with
        | ValueNone -> promoteDigit left
        | ValueSome (node, rest) ->
            Deep (left, rest, Node.toDigit node)
    
    | Deep(left, middle, Two (b, _)) ->
        Deep (left, middle, One b)
    
    | Deep(left, middle, Three (c, b, _)) ->
        Deep (left, middle, Two (c, b))
    
    | Deep(left, middle, Four (d, c, b, _)) ->
        Deep (left, middle, Three (d, c, b))

/// <summary>
/// Returns the leftmost leaf of the tree and a new tree that is missing that leaf.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The leftmost leaf of the tree and a new tree that is missing that leaf.</returns>
let popLeft (tree: FingerTree<'t>) =
    match tree with
    | Blank -> None, Blank
    | Single x -> Some x, Blank
    
    | Deep(One x, middle, right) ->
        let rest =
            match viewV middle with
            | ValueNone -> promoteDigit right
            | ValueSome (node, rest) ->
                Deep (Node.toDigit node, rest, right)

        Some x, rest

    | Deep(Two (a, b), middle, right) ->
        Some a, Deep (One b, middle, right)
    
    | Deep(Three (a, b, c), middle, right) ->
        Some a, Deep (Two (b, c), middle, right)
    
    | Deep(Four (a, b, c, d), middle, right) ->
        Some a, Deep (Three (b, c, d), middle, right)   
/// <summary>
/// Returns the leftmost leaf of the tree and a new tree that is missing that leaf.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The leftmost leaf of the tree and a new tree that is missing that leaf.</returns>
let popLeftV (tree: FingerTree<'t>) =
    match tree with
    | Blank -> ValueNone, Blank
    | Single x -> ValueSome x, Blank
    
    | Deep(One x, middle, right) ->
        let rest =
            match viewV middle with
            | ValueNone -> promoteDigit right
            | ValueSome (node, rest) ->
                Deep (Node.toDigit node, rest, right)

        ValueSome x, rest

    | Deep(Two (a, b), middle, right) ->
        ValueSome a, Deep (One b, middle, right)
    
    | Deep(Three (a, b, c), middle, right) ->
        ValueSome a, Deep (Two (b, c), middle, right)
    
    | Deep(Four (a, b, c, d), middle, right) ->
        ValueSome a, Deep (Three (b, c, d), middle, right)
        
/// <summary>
/// Returns the rightmost leaf of the tree and a new tree that is missing that leaf.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The rightmost leaf of the tree and a new tree that is missing that leaf.</returns>
let popRight (tree: FingerTree<'t>) =
    match tree with
    | Blank -> None, Blank
    | Single x -> Some x, Blank
    
    | Deep(left, middle, One x) ->
        let rest =
            match viewV middle with
            | ValueNone -> promoteDigit left
            | ValueSome (node, rest) ->
                Deep (left, rest, Node.toDigit node)

        Some x, rest
    
    | Deep(left, middle, Two (b, a)) ->
        Some a, Deep (left, middle, One b)
    
    | Deep(left, middle, Three (c, b, a)) ->
        Some a, Deep (left, middle, Two (c, b))
    
    | Deep(left, middle, Four (d, c, b, a)) ->
        Some a, Deep (left, middle, Three (d, c, b))
/// <summary>
/// Returns the rightmost leaf of the tree and a new tree that is missing that leaf.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The rightmost leaf of the tree and a new tree that is missing that leaf.</returns>
let popRightV (tree: FingerTree<'t>) =
    match tree with
    | Blank -> ValueNone, Blank
    | Single x -> ValueSome x, Blank
    
    | Deep(left, middle, One x) ->
        let rest =
            match viewV middle with
            | ValueNone -> promoteDigit left
            | ValueSome (node, rest) ->
                Deep (left, rest, Node.toDigit node)

        ValueSome x, rest
    
    | Deep(left, middle, Two (b, a)) ->
        ValueSome a, Deep (left, middle, One b)
    
    | Deep(left, middle, Three (c, b, a)) ->
        ValueSome a, Deep (left, middle, Two (c, b))
    
    | Deep(left, middle, Four (d, c, b, a)) ->
        ValueSome a, Deep (left, middle, Three (d, c, b))

/// <summary>
/// Appends the specified value to the left side of the tree.
/// </summary>
/// <param name="value">The value to insert.</param>
/// <param name="tree">The input tree.</param>
/// <returns>A tree containing all the the elements from the input tree with the specified value added.</returns>
let rec insertLeft<'t> (value: 't) (tree: FingerTree<'t>) : FingerTree<'t> =
    match tree with
    | Blank -> Single value
    | Single b -> Deep (One value, Blank, One b)

    | Deep (Four (b, c, d, e), middle, right) ->
        Deep (Two(value, b), insertLeft (Node3 (c, d, e)) middle, right)

    | Deep (left, middle, right) ->
        Deep (Digit.prepend value left, middle, right)
/// <summary>
/// Appends the specified value to the right side of the tree.
/// </summary>
/// <param name="value">The value to insert.</param>
/// <param name="tree">The input tree.</param>
/// <returns>A tree containing all the elements from the input tree with the specified value added.</returns>
let insertRight<'t> (value: 't) (tree: FingerTree<'t>): FingerTree<'t> =
    FingerTreeShared.insertRight value tree

/// Determines if the given tree meets the required definition of shallowness.
/// This is often used to swap between two different algorithms:
/// One that is faster on small trees, and one that is better suited for big trees.
let rec isShallow<'t> (tree: FingerTree<'t>) (depth: int) (threshold: int): bool =
    match tree with
    | Blank -> true
    | Single _ -> true
    | Deep (_, Blank, _) -> true
    | Deep (_, middle, _) ->
        if depth + 1 > threshold then
            false
        else
            isShallow middle (depth + 1) threshold

/// <summary>
/// Applies the specified function to every element in the tree in reverse order.
/// </summary>
/// <param name="action">The action to apply.</param>
/// <param name="tree">The input tree.</param>
let rec iterBack (action: 't -> unit) (tree: FingerTree<'t>) =
    match viewRevV tree with
    | ValueNone -> ()
    | ValueSome (value, rest) ->
        action value
        iterBack action rest

/// <summary>
/// Applies the specified function to the pair of every element in the tree and it's matching index in reverse order.
/// </summary>
/// <param name="action">The action to apply.</param>
/// <param name="tree">The input tree.</param>
let iterBacki (action: int -> 't -> unit) (tree: FingerTree<'t>) =
    let rec go (action: OptimizedClosures.FSharpFunc<_, _, _>) index rest =
        match viewRevV rest with
        | ValueNone -> ()
        | ValueSome (value, rest) ->
            action.Invoke (index, value)
            go action (index + 1) rest
    
    go (OptimizedClosures.FSharpFunc<_, _, _>.Adapt action) 0 tree

// Core

/// <summary>
/// Applies a function to each element of the tree, threading an accumulator argument through the computation.
/// Take the second argument, and apply the function to it and the first element of the tree.
/// Then feed this result into the function along with the second element and so on.
/// If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes <c>f (... (f s i0) i1 ...) iN</c>.
/// </summary>
/// <param name="folder">The function to update the state given the input elements.</param>
/// <param name="state">The initial state.</param>
/// <param name="tree">The input tree.</param>
let rec fold (folder: 'state -> 't -> 'state) (state: 'state) (tree: FingerTree<'t>) : 'state =
    match viewV tree with
    | ValueNone -> state
    | ValueSome (value, rest) ->
        fold folder (folder state value) rest

/// <summary>
/// Applies a function to each element of the tree starting from the end, threading an accumulator argument through the computation.
/// Take the third argument, and apply the function to it and the last element of the tree.
/// Then feed this result into the function along with the second to last element and so on.
/// If the input function is <c>f</c> and the elements are <c>i0...iN</c> then computes <c>f i0 (... (f iN s)) iN</c>.
/// </summary>
/// <param name="folder">The function to update the state given the input elements.</param>
/// <param name="state">The initial state.</param>
/// <param name="tree">The input tree.</param>
let rec foldBack (folder: 't -> 'state -> 'state) (tree: FingerTree<'t>) (state: 'state) =
    match viewRevV tree with
    | ValueNone -> state
    | ValueSome (value, rest) ->
        foldBack folder rest (folder value state)

/// <summary>
/// Applies the specified function to every element in the tree.
/// </summary>
/// <param name="action">The action to apply.</param>
/// <param name="tree">The input tree.</param>
let rec iter (action: 't -> unit) (tree: FingerTree<'t>) =
    match viewV tree with
    | ValueNone -> ()
    | ValueSome (value, rest) ->
        action value
        iter action rest

/// <summary>
/// Converts the tree into a sequence.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>A sequence with all the items contained by the tree and in the same order.</returns>
let toSeq tree =
    Seq.unfold (fun rest ->
        match viewV rest with
        | ValueSome (value, rest) -> Some (value, rest)
        | ValueNone -> None
    ) tree

/// <summary>
/// Creates a finger tree from the given sequence.
/// </summary>
/// <param name="sequence">The input sequence.</param>
/// <returns>A finger tree containing all the same items as the given sequence.</returns>
let ofSeq (sequence: #seq<'t>) =
    sequence
    |> Seq.fold (fun tree item -> insertRight item tree) Blank

/// <summary>
/// Returns a new finger tree that contains all pairings of elements from the first and second trees.
/// </summary>
/// <param name="a">The first tree to fetch items from.</param>
/// <param name="b">The second tree to fetch items from.</param>
/// <returns>All pairings of elements from <paramref name="a"/> and <paramref name="b"/>.</returns>
let allPairs (a: 't FingerTree) (b: 'u FingerTree) =
    let s1 = toSeq a
    let s2 = toSeq b
    
    Seq.allPairs s1 s2
    |> ofSeq

/// <summary>
/// Concatenates two finger trees into a single larger tree.
/// </summary>
/// <param name="a">The first tree to merge.</param>
/// <param name="b">The second tree to merge.</param>
/// <returns>The merged trees.</returns>
let append a b =
    b
    |> fold (fun result item ->
        insertRight item result
    ) a

/// <summary>
/// Returns the average of the elements in the sequence.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The average.</returns>
/// <exception cref="System.InvalidArgumentException">The input tree was empty.</exception>
let inline average (tree: 't FingerTree) : 't when ^t: (static member (+): ^t * ^t -> ^t) and ^t: (static member DivideByInt: ^t * int -> ^t) and ^t: (static member Zero: ^t)=
    let mutable total = 't.Zero
    let mutable count = 0
    
    iter (fun v ->
        total <- total + v
        count <- count + 1
    ) tree
    
    if count = 0 then invalidArg (nameof tree) "The input tree was empty."
    't.DivideByInt(total, count)

/// <summary>
/// Returns the average of the elements in the sequence.
/// </summary>
/// <param name="projection">A function applied to transform each element of the tree.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The average.</returns>
let inline averageBy ([<InlineIfLambda>] projection: 't -> 'u) (tree: 't FingerTree) : 'u when ^u: (static member (+): ^u * ^u -> ^u) and ^u: (static member DivideByInt: ^u * int -> ^u) and ^u: (static member Zero: ^u) =
    let mutable total = 'u.Zero
    let mutable count = 0
    
    iter (fun v ->
        total <- total + projection v
        count <- count + 1
    ) tree
    
    if count = 0 then invalidArg (nameof tree) "The input tree was empty."
    'u.DivideByInt(total, count)

/// <summary>Applies the given function to each element of the tree. Returns a tree comprised of the results "x" for each element where the function returns Some(x).</summary>
/// <param name="chooser">A function to transform items of type T into options of type U.</param>
/// <param name="source">The input tree.</param>
/// <returns>The resulting tree.</returns>
let choose (chooser: 't -> 'u option) (source: FingerTree<'t>) =
    fold (fun state item ->
        match chooser item with
        | Some value -> insertRight value state
        | None -> state
    ) Blank source
/// <summary>Applies the given function to each element of the tree. Returns a tree comprised of the results "x" for each element where the function returns ValueSome(x).</summary>
/// <param name="chooser">A function to transform items of type T into options of type U.</param>
/// <param name="source">The input tree.</param>
/// <returns>The resulting tree.</returns>
let chooseV (chooser: 't -> 'u ValueOption) (source: FingerTree<'t>) =
    fold (fun state item ->
        match chooser item with
        | ValueSome value -> insertRight value state
        | ValueNone -> state
    ) Blank source

/// <summary>Divides the input sequence into chunks of size at most <c>chunkSize</c>.</summary>
/// <param name="chunkSize">The maximum size of each chunk.</param>
/// <param name="source">The input sequence.</param>
/// <returns>The sequence divided into chunks.</returns>
/// <exception cref="T:System.ArgumentException">Thrown when <c>chunkSize</c> is not positive.</exception>
let chunkBySize chunkSize source =
    let result, index =
        source
        |> fold (fun (result, index) item ->
            match right result with
            | Some (group: 't[]) ->
                if index < group.Length then
                    group[index] <- item
                    result, index + 1   
                else
                    let newGroup = Array.zeroCreate chunkSize
                    newGroup[0] <- item
                    (insertRight newGroup result), 1
            | None -> result, index
        ) (Single (Array.zeroCreate chunkSize), 0)
    
    match popRight result with
    | Some group, rest ->
        if index < group.Length then
            let finalGroup = Array.zeroCreate index
            Array.Copy(group, finalGroup, index)
            
            insertRight finalGroup rest
        else
            result
    | None, _ ->
        result

/// <summary>Applies the given function to each element of the tree and concatenates all the results.</summary>
/// <param name="mapping">A function to transform elements of the input tree into the trees that will then be concatenated.</param>
/// <param name="source">The input tree.</param>
/// <returns>The result tree.</returns>
let collect mapping source =
    source
    |> fold (fun result item -> append result (mapping item)) Blank

/// <summary>Compares two sequences using the given comparison function, element by element.</summary>
///
/// <param name="comparer">A function that takes an element from each sequence and returns an int.
/// If it evaluates to a non-zero value iteration is stopped and that value is returned.</param>
/// <param name="source1">The first input sequence.</param>
/// <param name="source2">The second input sequence.</param>
///
/// <returns>Returns the first non-zero result from the comparison function.  If the end of a sequence
/// is reached it returns a -1 if the first sequence is shorter and a 1 if the second sequence
/// is shorter.</returns>
let compareWith comparer source1 source2 =
    let rec compare viewA viewB =
        match viewV viewA, viewV viewB with
        | ValueNone, ValueNone -> 0
        | ValueNone, _ -> -1
        | _, ValueNone -> 1
        | ValueSome (itemA, restA), ValueSome (itemB, restB) ->
            let result = comparer itemA itemB
            
            if result <> 0 then result
            else
                compare restA restB

    compare source1 source2

/// <summary>Combines the given tree-of-trees as a single concatenated tree.</summary>
/// <param name="sources">The input tree-of-trees.</param>
/// <returns>The result tree.</returns>
let concat sources = collect id sources

/// <summary>
/// Tests if the tree contains the specified value.
/// </summary>
/// <param name="value">The value to search for.</param>
/// <param name="source">The tree to search through.</param>
/// <returns>True if the tree contains the specified value.</returns>
let contains value source =
    let rec find value source =
        match popLeft source with
        | Some treeValue, rest ->
            if treeValue = value then true
            else find value rest
        | None, _ -> false
    
    find value source

/// <summary>Applies a key-generating function to each element of a tree and returns a tree yielding unique keys and their number of occurrences in the original tree.</summary>
/// <param name="projection">A function transforming each item of the input tree into a key to be compared against the others.</param>
/// <param name="source">The input tree.</param>
let countBy projection source =
    source
    |> fold (fun counts item ->
        let key = projection item
        
        match counts |> Map.tryFind key with
        | Some existing ->
            counts |> Map.add key (existing + 1)
        | None ->
            counts |> Map.add key 1
    ) Map.empty
    |> ofSeq

/// <summary>
/// Returns the first element of the tree or <c>None</c> if the tree is empty.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The first element of the tree or <c>None</c>.</returns>
let tryHead tree = left tree
/// <summary>
/// Returns the first element of the tree or <c>ValueNone</c> if the tree is empty.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The first element of the tree or <c>ValueNone</c>.</returns>
let tryHeadV tree : 't voption = leftV tree

/// <summary>
/// Returns the first <c>x</c> where the given function returns <c>Some(x)</c>.
/// </summary>
/// <param name="chooser">The transformer function.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The first <c>x</c> where the given function returns <c>Some(x)</c>.</returns>
let tryPick chooser tree =
    let rec pick chooser tree =
        match viewV tree with
        | ValueNone -> None
        | ValueSome (item, rest) ->
            match chooser item with
            | Some _ as result -> result
            | None ->
                pick chooser rest
    
    pick chooser tree
/// <summary>
/// Returns the first <c>x</c> where the given function returns <c>ValueSome(x)</c>.
/// </summary>
/// <param name="chooser">The transformer function.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The first <c>x</c> where the given function returns <c>ValueSome(x)</c>.</returns>
let tryPickV chooser tree =
    let rec pick chooser tree =
        match viewV tree with
        | ValueNone -> ValueNone
        | ValueSome (item, rest) ->
            match chooser item with
            | ValueSome _ as result -> result
            | ValueNone ->
                pick chooser rest
    
    pick chooser tree

/// <summary>
/// Returns the first x where the given function returns <c>Some(x)</c>.
/// </summary>
/// <param name="chooser">The transformer function.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The first <c>x</c> where the given function returns <c>Some(x)</c>.</returns>
/// <exception cref="Functional.NoSuchItemException">Thrown if every result from <c>chooser</c> is <c>None</c>.</exception>
let pick chooser tree =
    match tryPick chooser tree with
    | Some x -> x
    | None -> noSuchItem "The choice function returned None for all of the items in the input tree."       
/// <summary>
/// Returns the first x where the given function returns <c>ValueSome(x)</c>.
/// </summary>
/// <param name="chooser">The transformer function.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The first <c>x</c> where the given function returns <c>ValueSome(x)</c>.</returns>
/// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown if every result from <c>chooser</c> is <c>ValueNone</c>.</exception>
let pickV chooser tree =
    match tryPickV chooser tree with
    | ValueSome x -> x
    | ValueNone -> noSuchItem "The choice function returned ValueNone for all of the items in the input tree."

/// <summary>
/// Returns a tree with all entries in the same order as the original, but with no duplicate entries.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>A tree with all entries in the same order as the original, but with no duplicate entries.</returns>
let distinct tree =
    tree
    |> fold (fun (result, seen) item ->
        if seen |> Set.contains item then
            result, seen
        else
            insertRight item result, Set.add item seen
    ) (Blank, Set.empty)
    |> fst
/// <summary>
/// Returns a tree with all entries in the same order as the original, but with no duplicate entries according to the keys generated for each entry by the given function.
/// </summary>
/// <param name="projection">A function that transforms the tree items into comparable keys.</param>
/// <param name="tree">The input tree.</param>
/// <returns>A tree with all entries in the same order as the original, but with no duplicate entries.</returns>
let distinctBy projection tree =
    tree
    |> fold (fun (result, seen) item ->
        let key = projection item
        
        if seen |> Set.contains key then
            result, seen
        else
            insertRight item result, Set.add key seen
    ) (Blank, Set.empty)
    |> fst

/// <summary>
/// Returns the length of the tree. This function requires O(n) time, so caching the result if used repeatedly is recomended.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The length of the tree.</returns>
let length (tree: FingerTree<'t>) = 
    let rec count total (tree: FingerTree<'t>) =
        match tree with
        | Blank -> 0
        | Single _ -> 1
        
        | Deep(One _, middle, right) ->
            let rest =
                match viewV middle with
                | ValueNone -> promoteDigit right
                | ValueSome (node, rest) ->
                    Deep (Node.toDigit node, rest, right)
            
            count (total + 1) rest
        
        | Deep(Two _, middle, right) ->
            let rest =
                match viewV middle with
                | ValueNone -> promoteDigit right
                | ValueSome (node, rest) ->
                    Deep (Node.toDigit node, rest, right)
            
            count (total + 2) rest
        
        | Deep(Three _, middle, right) ->
            let rest =
                match viewV middle with
                | ValueNone -> promoteDigit right
                | ValueSome (node, rest) ->
                    Deep (Node.toDigit node, rest, right)
            
            count (total + 3) rest
        
        | Deep(Four _, middle, right) ->
            let rest =
                match viewV middle with
                | ValueNone -> promoteDigit right
                | ValueSome (node, rest) ->
                    Deep (Node.toDigit node, rest, right)
            
            count (total + 4) rest
        
    count 0 tree

/// <summary>
/// Splits the input tree into at most <c>count</c> chunks.
/// </summary>
/// <param name="count">The maximum number of chunks.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The tree split into chunks.</returns>
let splitInto count tree =
    let treeSize = length tree
    let chunkSize = treeSize / count + (if treeSize % count <> 0 then 1 else 0)
    chunkBySize chunkSize tree

/// An empty finger tree.
let empty<'t> = Blank : FingerTree<'t>

/// <summary>
/// Returns the only element of the tree.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <exception cref="System.ArgumentException">Thrown when the tree does not have precisely one element.</exception>
let exactlyOne tree =
    match tree with
    | Single v -> v
    | _ ->
        invalidArg (nameof tree) "The given tree did not contain exactly one element."

/// <summary>
/// Returns the only element of the tree or <c>None</c> if the tree does not contain precisely one element.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The only element of the tree or <c>None</c> if the tree does not contain precisely one element.</returns>
let tryExactlyOne tree =
    match tree with
    | Single v -> Some v
    | _ -> None
/// <summary>
/// Returns the only element of the tree or <c>ValueNone</c> if the tree does not contain precisely one element.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The only element of the tree or <c>ValueNone</c> if the tree does not contain precisely one element.</returns>
let tryExactlyOneV tree =
    match tree with
    | Single v -> ValueSome v
    | _ -> ValueNone

/// <summary>
/// Returns a new tree that contains all the items except any which appear in <c>excluded</c>.
/// </summary>
/// <param name="excluded">The items to exclude.</param>
/// <param name="tree">The input tree.</param>
/// <returns>Returns a new tree that contains all the items except any which appear in <c>excluded</c>.</returns>
/// <exception cref="System.ArgumentNullException">Thrown when excluded is null.</exception>
let except excluded tree =
    ensureNotNull (nameof excluded) excluded
    let excluded = Set.ofSeq excluded
    
    tree
    |> fold (fun result item ->
        if excluded |> Set.contains item then
            result
        else
            insertRight item result
    ) empty

/// <summary>
/// Tests if any element of the tree satisfies the given predicate.
/// </summary>
/// <param name="predicate">The function to test the input elements.</param>
/// <param name="tree">The input tree.</param>
/// <returns>True if any result from <c>predicate</c> is true.</returns>
let rec exists predicate tree =
    match viewV tree with
    | ValueNone -> false
    | ValueSome (item, rest) ->
        if predicate item then true
        else exists predicate rest

/// <summary>
/// Tests if any pair of corresponding element of the trees satisfies the given predicate.
/// </summary>
/// <param name="predicate">The function to test the input elements.</param>
/// <param name="tree1">The first input tree.</param>
/// <param name="tree2">The second input tree.</param>
/// <returns>True if any result from <c>predicate</c> is true.</returns>
let rec exists2 predicate tree1 tree2 =
    let rec go (predicate: OptimizedClosures.FSharpFunc<_, _, _>) tree1 tree2 =
        match viewV tree1, viewV tree2 with
        | ValueNone, ValueNone -> false
        | ValueNone, _ | _, ValueNone  -> invalidArgNotOfEqualLength "tree"
        
        | ValueSome (itemA, restA), ValueSome (itemB, restB) ->
            if predicate.Invoke (itemA, itemB) then true
            else go predicate restA restB
    
    go (OptimizedClosures.FSharpFunc<_, _, _>.Adapt predicate) tree1 tree2

/// <summary>
/// Returns a new tree containing only those items for which the specified condition returned true.
/// </summary>
/// <param name="condition">The condition to check each item against.</param>
/// <param name="tree">The input tree.</param>
let filter (condition: 't -> bool) (tree: FingerTree<'t>) =
    fold (fun state item ->
        if condition item then
            insertRight item state
        else
            state
    ) Blank tree

/// <summary>
/// Returns the first element for which the given function returns 'true'.
/// </summary>
/// <param name="predicate">The function to test each element with.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The first element for which the given function returns 'true'.</returns>
/// <exception cref="Functional.NoSuchItemException">Thrown if <c>predicate</c> never returns true.</exception>
let rec find predicate tree =        
    match viewV tree with
    | ValueNone ->
        noSuchItem "An item satisfying the predicate was not found in the collection."
    | ValueSome (item, rest) ->
        if predicate item then
            item
        else
            find predicate rest

/// <summary>
/// Returns the last element for which the given function returns 'true'.
/// </summary>
/// <param name="predicate">The function to test each element with.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The last element for which the given function returns 'true'.</returns>
/// <exception cref="Functional.NoSuchItemException">Thrown if <c>predicate</c> never returns true.</exception>
let rec findBack predicate tree = 
    match viewRevV tree with
    | ValueNone ->
        noSuchItem "An item satisfying the predicate was not found in the collection."
    | ValueSome (item, rest) ->
        if predicate item then
            item
        else
            findBack predicate rest

/// <summary>
/// Returns the index of the first element for which the given function returns 'true'.
/// </summary>
/// <param name="predicate">The function to test each element with.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The index of the first element for which the given function returns 'true'.</returns>
/// <exception cref="Functional.NoSuchItemException">Thrown if <c>predicate</c> never returns true.</exception>
let findIndex predicate tree =
    let rec find predicate tree index =
        match viewV tree with
        | ValueNone ->
            noSuchItem "An item satisfying the predicate was not found in the collection."
        | ValueSome (item, rest) ->
            if predicate item then
                index
            else
                find predicate rest (index + 1)
                
    find predicate tree 0
    
/// <summary>
/// Returns the index of the last element for which the given function returns 'true'.
/// </summary>
/// <param name="predicate">The function to test each element with.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The index of the last element for which the given function returns 'true'.</returns>
/// <exception cref="Functional.NoSuchItemException">Thrown if <c>predicate</c> never returns true.</exception>
let findIndexBack predicate tree =
    let rec find predicate tree index =
        match viewRevV tree with
        | ValueNone ->
            noSuchItem "An item satisfying the predicate was not found in the collection."
        | ValueSome (item, rest) ->
            if predicate item then
                index
            else
                find predicate rest (index - 1)
                
    find predicate tree (length tree - 1)
    
/// <summary>
/// Tests if all elements of the tree satisfy the given predicate.
/// </summary>
/// <param name="predicate">The predicate to test each element with.</param>
/// <param name="tree">The input tree.</param>
/// <returns>True if all elements of the tree satisfy the given predicate.</returns>
let rec forall predicate tree =
    match viewV tree with
    | ValueNone -> true
    | ValueSome (item, rest) ->
        if predicate item then
            forall predicate rest
        else
            false

/// <summary>
/// Tests if all corresponding elements of the two trees satisfy the given predicate.
/// </summary>
/// <param name="predicate">The predicate to test each element with.</param>
/// <param name="tree1">The first input tree.</param>
/// <param name="tree2">The second input tree</param>
/// <returns>True if all corresponding elements of the two trees satisfy the given predicate.</returns>
/// <exception cref="System.ArgumentException">The two trees were not of equal length.</exception>
let forall2 predicate tree1 tree2 =
    let rec go (predicate: OptimizedClosures.FSharpFunc<_, _, _>) tree1 tree2 =
        match viewV tree1, viewV tree2 with
        | ValueNone, ValueNone -> true
        | ValueNone, _ | _, ValueNone -> invalidArgNotOfEqualLength "tree"

        | ValueSome (itemA, restA), ValueSome(itemB, restB) ->
            if predicate.Invoke (itemA, itemB) then
                go predicate restA restB
            else
                false
    
    go (OptimizedClosures.FSharpFunc<_, _, _>.Adapt predicate) tree1 tree2

/// <summary>
/// Applies a function to pairs of elements drawn from the two collections, left-to-right, threading an accumulator argument through the computation.
/// </summary>
/// <param name="folder">The function to update the state given the input elements.</param>
/// <param name="state">The initial state.</param>
/// <param name="tree1">The first input tree.</param>
/// <param name="tree2">The second input tree.</param>
/// <returns>The final state.</returns>
/// <exception cref="System.ArgumentException">The two trees differed in length.</exception>
let fold2 folder (state: 'state) (tree1: 'a FingerTree) (tree2: 'b FingerTree) =
    let rec go (folder: OptimizedClosures.FSharpFunc<_, _, _, _>) state tree1 tree2 =
        match viewV tree1, viewV tree2 with
        | ValueNone, ValueNone -> state
        | ValueNone, _ | _, ValueNone ->
            invalidArgNotOfEqualLength "tree"
            
        | ValueSome (itemA, restA), ValueSome (itemB, restB) ->
            go folder (folder.Invoke (state, itemA, itemB)) restA restB
    
    go (OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt folder) state tree1 tree2

/// <summary>
/// Applies a function to pairs of elements drawn from the two collections, right-to-right, threading an accumulator argument through the computation.
/// </summary>
/// <param name="folder">The function to update the state given the input elements.</param>
/// <param name="state">The initial state.</param>
/// <param name="tree1">The first input tree.</param>
/// <param name="tree2">The second input tree.</param>
/// <returns>The final state.</returns>
/// <exception cref="System.ArgumentException">The two trees differed in length.</exception>
let foldBack2 folder tree1 tree2 (state: 'state) =
    let rec go (folder: OptimizedClosures.FSharpFunc<_, _, _, _>) tree1 tree2 state =
        match viewRevV tree1, viewRevV tree2 with
        | ValueNone, ValueNone -> state
        | ValueNone, _ | _, ValueNone ->
            invalidArgNotOfEqualLength "tree"
            
        | ValueSome (itemA, restA), ValueSome (itemB, restB) ->
            go folder restA restB (folder.Invoke (state, itemA, itemB))
    
    go (OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt folder) tree1 tree2 state
      
/// <summary>
/// Returns the first element of the tree.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The first element of the tree.</returns>
/// <exception cref="System.ArgumentException">Thrown when the input tree is empty.</exception>
let head tree =
    match leftV tree with
    | ValueSome x -> x
    | ValueNone -> invalidArgCollectionWasEmpty()

/// <summary>
/// Applies a key-generating function to each element of a tree and yields a tree of unique keys.
/// Each unique key contains a tree of all elements that match to this key.
/// </summary>
/// <param name="projection">A function that transforms an element of the tree into a comparable key.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The result tree.</returns>
let groupBy projection tree =
    tree
    |> fold (fun groups item ->
        groups
        |> Map.change (projection item) (fun existing ->
            insertRight item (existing |> Option.defaultValue empty)
            |> Some
        )
    ) Map.empty
    |> Seq.map (fun (KeyValue (k, v)) -> k, v)
    |> ofSeq

/// <summary>
/// Transforms each element of tree while threading an accumulator through the computation.
/// </summary>
/// <param name="mapping">The transformation function to apply.</param>
/// <param name="state">The initial state.</param>
/// <param name="tree">The input tree.</param>
/// <returns>A pairing of the transformed elements and the final state.</returns>
let mapFold (mapping: 'state -> 't -> 'u * 'state) (state: 'state) (tree: FingerTree<'t>) =
    fold (fun (result, state) item ->
        let item, newState = mapping state item
        (insertRight item result, newState)
    ) (Blank, state) tree

/// <summary>
/// Returns a new tree that contains each element paired with its corresponding index.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>A new tree that contains each element paired with its corresponding index.</returns>
let indexed tree =
    tree
    |> mapFold (fun index item -> (index, item), (index + 1)) 0

/// <summary>
/// Creates a finger tree with the given length and generator function to compute the elements.
/// </summary>
/// <param name="count">The number of elements to generate.</param>
/// <param name="initializer">The function to generate the value for each index.</param>
let init count initializer =
    let rec make tree index count initializer =
        if index = count then tree
        else
            let value = initializer index
            make (insertRight value tree) (index + 1) count initializer
    
    make empty 0 count initializer

/// <summary>
/// Determines whether the given tree is empty.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>True if the given tree is empty</returns>
let isEmpty (tree: FingerTree<'t>) = tree.IsEmpty    

/// <summary>
/// Applies the given function to each pair of corresponding elements form the two trees.
/// </summary>
/// <param name="action">The action to apply to each pair.</param>
/// <param name="tree1">The first input tree.</param>
/// <param name="tree2">The second input tree.</param>
/// <exception cref="System.ArgumentException">The two trees differ in length.</exception>
let iter2 (action: 't1 -> 't2 -> unit) tree1 tree2 =
    let rec go (action: OptimizedClosures.FSharpFunc<_, _, _>) tree1 tree2 =
        match viewV tree1, viewV tree2 with
        | ValueNone, ValueNone -> ()
        | ValueNone, _ | _, ValueNone ->
            invalidArgNotOfEqualLength "tree"
            
        | ValueSome (itemA, restA), ValueSome (itemB, restB) ->
            action.Invoke(itemA, itemB)
            go action restA restB
    
    go (OptimizedClosures.FSharpFunc<_, _, _>.Adapt action) tree1 tree2

/// <summary>
/// Applies the specified function to the pair of every element in the tree and it's matching index.
/// </summary>
/// <param name="action">The action to apply.</param>
/// <param name="tree">The input tree.</param>
let iteri (action: int -> 't -> unit) (tree: FingerTree<'t>) =
    let rec go (action: OptimizedClosures.FSharpFunc<_, _, _>) index rest =
        match viewV rest with
        | ValueNone -> ()
        | ValueSome (value, rest) ->
            action.Invoke (index, value)
            go action (index + 1) rest
    
    go (OptimizedClosures.FSharpFunc<_, _, _>.Adapt action) 0 tree
    
/// <summary>
/// Applies the specified function to the triple of every corresponding element in the two trees and their matching index.
/// </summary>
/// <param name="action">The action to apply.</param>
/// <param name="tree1">The first input tree.</param>
/// <param name="tree2">The second input tree.</param>
/// <exception cref="System.ArgumentException">The two trees differ in length.</exception>
let iteri2 (action: int -> 't1 -> 't2 -> unit) tree1 tree2 =
    let rec go (action: OptimizedClosures.FSharpFunc<_, _, _, _>) index tree1 tree2 =
        match viewV tree1, viewV tree2 with
        | ValueNone, ValueNone -> ()
        | ValueNone, _ | _, ValueNone ->
            invalidArgNotOfEqualLength "tree"
        
        | ValueSome (itemA, restA), ValueSome(itemB, restB) ->
             action.Invoke(index, itemA, itemB)
             go action (index + 1) restA restB
    
    go (OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt action) 0 tree1 tree2

/// <summary>
/// Returns the last element of the tree.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The last element of the tree.</returns>
/// <exception cref="System.ArgumentException">Thrown when the input does not have any elements.</exception>
let last tree =
    match rightV tree with
    | ValueSome x -> x
    | ValueNone -> invalidArgCollectionWasEmpty()

/// <summary>
/// Fetches the item at the specified index.
/// This function is slow and requires O(index) time to find the item.
/// </summary>
/// <param name="index">The index of the item to fetch.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The item at the specified index.</returns>
/// <exception cref="System.IndexOutOfRangeException">The index is less than zero or greater than the size of the tree.</exception>
let rec item index tree =
    match tree with
    | Blank -> indexOutOfRangeMustBeWithinCollection()
    | Single x -> if index = 0 then x else indexOutOfRangeMustBeWithinCollection()
        
    | Deep(left, _, _) ->
        match left with
        | One a when index = 0 -> a
        
        | Two(a, _) when index = 0 -> a
        | Two(_, b) when index = 1 -> b
        
        | Three(a, _, _) when index = 0 -> a
        | Three(_, b, _) when index = 1 -> b
        | Three(_, _, c) when index = 2 -> c
        
        | Four(a, _, _, _) when index = 0 -> a
        | Four(_, b, _, _) when index = 1 -> b
        | Four(_, _, c, _) when index = 2 -> c
        | Four(_, _, _, d) when index = 3 -> d
        
        | _ ->
            match leftV tree with
            | ValueNone -> indexOutOfRangeMustBeWithinCollection()
            | ValueSome value ->
                if index = 0 then value else item (index - 1) (removeLeft tree)

/// <summary>
/// Returns the last element of the tree or <c>None</c> if no such element exists.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The last element of the tree or <c>None</c> if no such element exists.</returns>
let tryLast tree = right tree
/// <summary>
/// Returns the last element of the tree or <c>ValueNone</c> if no such element exists.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The last element of the tree or <c>ValueNone</c> if no such element exists.</returns>
let tryLastV tree = rightV tree

let rec mapShallow<'t, 'u> (mapping: 't -> 'u) (tree: FingerTree<'t>): FingerTree<'u> =
    match tree with
    | Blank -> Blank
    | Single value ->
        Single (mapping value)
        
    | Deep (left, Blank, right) ->
        Deep (Digit.map mapping left, Blank, Digit.map mapping right)

    | Deep (left, middle, right) ->
        Deep (
            Digit.map mapping left,
            mapShallow (Node.map mapping) middle,
            Digit.map mapping right
        )
let private mapDeep (mapping: 't -> 'u) (tree: FingerTree<'t>):  FingerTree<'u> =
    fold (fun state item ->
        insertRight (mapping item) state
    ) Blank tree
/// <summary>
/// Creates a new finger tree where each element is the result of applying the specified mapping function to each corresponding element in the input tree.
/// </summary>
/// <param name="mapping">The function to apply to each element.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The resulting tree.</returns>
let map (mapping: 't -> 'u) (tree: FingerTree<'t>): FingerTree<'u> =
    if isShallow tree 0 5 then
        mapShallow mapping tree
    else
        mapDeep mapping tree

let map2 mapping tree1 tree2 =
    let rec go (mapping: OptimizedClosures.FSharpFunc<_, _, _>) current tree1 tree2 =
        match viewV tree1, viewV tree2 with
        | ValueNone, ValueNone -> current
        | ValueNone, _ | _, ValueNone -> invalidArgNotOfEqualLength "tree"
        
        | ValueSome (itemA, restA), ValueSome (itemB, restB) ->
            go mapping (insertRight (mapping.Invoke (itemA, itemB)) current) restA restB
    
    go (OptimizedClosures.FSharpFunc<_, _, _>.Adapt mapping) empty tree1 tree2

/// <summary>
/// Transforms each element of tree from right to left while threading an accumulator through the computation.
/// </summary>
/// <param name="mapping">The transformation function to apply.</param>
/// <param name="state">The initial state.</param>
/// <param name="tree">The input tree.</param>
/// <returns>A pairing of the transformed elements and the final state.</returns>
let mapFoldBack (mapping: 'state -> 't -> 'u * 'state) (tree: FingerTree<'t>) (state: 'state) =
    foldBack (fun item (result, state) ->
        let item, newState = mapping state item
        (insertLeft item result, newState)
    ) tree (Blank, state)

/// <summary>
/// Applies a transformation function to corresponding values from three collections at once.
/// </summary>
/// <param name="mapping">The transformation function to apply.</param>
/// <param name="tree1">The first tree.</param>
/// <param name="tree2">The second tree.</param>
/// <param name="tree3">The third tree.</param>
/// <exception cref="System.ArgumentException">Thrown when the input trees differ in length.</exception>
let map3 mapping tree1 tree2 tree3 =
    let rec go (mapping: OptimizedClosures.FSharpFunc<_, _, _, _>) current tree1 tree2 tree3 =
        match viewV tree1, viewV tree2, viewV tree3 with
        | ValueNone, ValueNone, ValueNone -> current
        | ValueNone, _, _ | _, ValueNone, _ | _, _, ValueNone ->
            invalidArgNotOfEqualLength "tree"
        | ValueSome (itemA, restA), ValueSome (itemB, restB), ValueSome (itemC, restC) ->
            go mapping (insertRight (mapping.Invoke (itemA, itemB, itemC)) current) restA restB restC
    
    go (OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt mapping) empty tree1 tree2 tree3

let mapi2 mapping tree1 tree2 =
    let rec go (mapping: OptimizedClosures.FSharpFunc<_, _, _>) current tree1 tree2 =
        match viewV tree1, viewV tree2 with
        | ValueNone, ValueNone -> current
        | ValueNone, _ | _, ValueNone ->
            invalidArgNotOfEqualLength "tree"
        | ValueSome (itemA, restA), ValueSome (itemB, restB) ->
            go mapping (insertRight (mapping.Invoke (itemA, itemB)) current) restA restB
    
    go (OptimizedClosures.FSharpFunc<_, _, _>.Adapt mapping) empty tree1 tree2

/// <summary>
/// Applies the given function to each element and its index.
/// </summary>
/// <param name="mapping">The transformation function to apply to each item.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The resulting tree.</returns>
let mapi mapping tree =
    let mapping = OptimizedClosures.FSharpFunc<_, _, _>.Adapt mapping
    
    tree
    |> mapFold (fun index item ->
        (index + 1, mapping.Invoke(index, item))
    ) 0

/// <summary>
/// Applies a function to each element of the array, threading an accumulator through the computation.
/// If the input function is <c>f</c> and the elements are <c>i0..iN</c> then it computes <c>f(... (f i0 i1)...) iN</c>.
/// </summary>
/// <param name="reduction">The function to apply to each element.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The final state.</returns>
let reduce reduction tree =
    match viewV tree with
    | ValueNone -> invalidArgCollectionWasEmpty()
    | ValueSome (value, rest) ->
        fold reduction value rest

/// <summary>
/// Returns the greatest element in the tree.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The greatest element in the tree according to <c>Operators.max</c>.</returns>
let max tree = reduce Operators.max tree

/// <summary>
/// Returns the greatest element in the tree compared by the keys produced by <c>projection</c>.
/// </summary>
/// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The greatest element in the tree according to <c>Operators.max</c> on the keys produced by <c>projection</c>.</returns>
let maxBy projection tree = reduce (fun a b -> Operators.max (projection a) (projection b)) tree

/// <summary>
/// Returns the smallest element in the tree.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The smallest element in the tree according to <c>Operators.min</c>.</returns>
let min tree = reduce Operators.min tree

/// <summary>
/// Returns the smallest element in the tree compared by the keys produced by <c>projection</c>.
/// </summary>
/// <param name="projection">The function to transform the elements into a type supporting comparison.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The smallest element in the tree according to <c>Operators.min</c> on the keys produced by <c>projection</c>.</returns>
let minBy projection tree = reduce (fun a b -> Operators.min (projection a) (projection b)) tree

/// <summary>
/// Creates a finger tree from the given list.
/// </summary>
/// <param name="list">The input list.</param>
/// <returns>A finger tree containing all the same items as the given list.</returns>
let ofList list =
    match list with
    | [] -> Blank
    | [ a ] -> Single a
    | [ a; b ] -> Deep(One a, Blank, One b)
    | [ a; b; c ] -> Deep(One a, Blank, Two (b, c))
    | [ a; b; c; d ] -> Deep(Two (a, b), Blank, Two (c, d))
    | _ ->
        list
        |> List.fold (fun tree item ->
            insertRight item tree
        ) Blank

/// <summary>
/// Creates a finger tree from the given array.
/// </summary>
/// <param name="array">The input array.</param>
/// <returns>A finger tree containing all the same items as the given array.</returns>
let ofArray (array: 't[]): FingerTree<'t> =
    match array with
    | [||] -> Blank
    | [| a |] -> Single a
    | [| a; b |] -> Deep(One a, Blank, One b)
    | [| a; b; c |] -> Deep(One a, Blank, Two (b, c))
    | [| a; b; c; d |] -> Deep(Two (a, b), Blank, Two (c, d))
    | _ ->
        array
        |> Array.fold (fun tree item ->
            insertRight item tree
        ) Blank

/// <summary>
/// Returns a tree of each element in the input tree and its predecessor, except the first element which is only returned as the predecessor of the second element.
/// </summary>
/// <param name="tree">The input tree</param>
/// <returns>A tree of each element in the input tree and its predecessor, except the first element which is only returned as the predecessor of the second element.</returns>
let pairwise tree =
    let rec go result previous tree =
        match viewV tree with
        | ValueNone -> result
        | ValueSome (item, rest) ->
            go (insertRight (previous, item) result) item rest

    match viewV tree with
    | ValueNone -> empty
    | ValueSome (first, rest) ->
        go empty first rest

/// <summary>
/// Splits the collection into two collections, containing the elements from which the given predicate returns "true" and "false" respectively.
/// </summary>
/// <param name="condition">The function test the input elements.</param>
/// <param name="tree">The input tree.</param>
/// <returns>A pair of trees, the first containing the elements the predicate evaluted to true and the second containing those evaluated to false.</returns>
let partition (condition: 't -> bool) (tree: FingerTree<'t>) =
    let rec partition condition evTrue evFalse tree =
        match viewV tree with
        | ValueNone -> evTrue, evFalse
        | ValueSome (item, rest) ->
            if condition item then
                partition condition (insertRight item evTrue) evFalse rest
            else
                partition condition evTrue (insertRight item evFalse) rest
    
    partition condition Blank Blank tree

/// <summary>
/// Converts the tree into an array.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>An array with all the items contained by the tree and in the same order.</returns>
let toArray tree =
    [|
        let mutable tree = tree
        
        while tree <> Blank do
            match viewV tree with
            | ValueNone -> tree <- Blank
            | ValueSome(item, rest) ->
                yield item
                tree <- rest
    |]

/// <summary>
/// Returns a tree with all elements permuted according to the specified permutation.
/// </summary>
/// <param name="indexMap">The function that maps input indices to output indices.</param>
/// <param name="tree">The input tree.</param>
let permute indexMap tree =
    Array.permute indexMap (toArray tree)
    |> ofArray

/// <summary>
/// Applies a function to each element of the array in reverse order, threading an accumulator through the computation.
/// If the input function is <c>f</c> and the elements are <c>i0..iN</c> then it computes <c>f i0 (... (f iN-1 iN)...)</c>.
/// </summary>
/// <param name="reduction">The function to apply to each element.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The final state.</returns>
let reduceBack reduction tree =
    match viewRevV tree with
    | ValueNone -> invalidArgCollectionWasEmpty()
    | ValueSome (value, rest) ->
        foldBack reduction rest value

/// <summary>
/// Creates a tree by replicating the given initial value.
/// </summary>
/// <param name="count">The number of times to repeat the value.</param>
/// <param name="initial">The value to replicate.</param>
let replicate count initial =
    let mutable value = empty
    
    for i in 1..count do
        value <- insertRight initial value
    
    empty

let rec private revShallow<'t> (tree: FingerTree<'t>): FingerTree<'t> =
    match tree with
    | Blank | Single _ -> tree
    | Deep (left, middle, right) ->
        Deep (Digit.rev right, revShallow middle, Digit.rev left)
let rec private revDeep (tree: FingerTree<'t>) =
    fold (fun state item ->
        insertLeft item state
    ) Blank tree
/// <summary>
/// Returns a new tree with the elements in reverse order.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The reversed tree.</returns>
let rev (tree: FingerTree<'t>) =
    if isShallow tree 0 5 then
        revShallow tree
    else
        revDeep tree

/// <summary>
/// Like <c>fold</c>, but returning the intermediary and final results.
/// </summary>
/// <param name="folder">The function to update the state given the input elements.</param>
/// <param name="state">The initial state.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The tree of state values.</returns>
let scan folder state tree =
    let rec go (folder: OptimizedClosures.FSharpFunc<_, _, _>) results state tree =
        match viewV tree with
        | ValueNone -> insertRight state results
        | ValueSome (item, remaining) ->
            let newState = folder.Invoke(state, item)
            go folder (insertRight state results) newState remaining
    
    go (OptimizedClosures.FSharpFunc<_, _, _>.Adapt folder) (Single state) state tree
    
/// <summary>
/// Like <c>foldBack</c>, but returning the intermediary and final results.
/// </summary>
/// <param name="folder">The function to update the state given the input elements.</param>
/// <param name="state">The initial state.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The tree of state values.</returns>
let scanBack folder tree state =
    let rec go (folder: OptimizedClosures.FSharpFunc<_, _, _>) results tree state =
        match viewRevV tree with
        | ValueNone -> insertLeft state results
        | ValueSome (item, remaining) ->
            let newState = folder.Invoke(item, state)
            go folder (insertLeft state results) remaining newState
    
    go (OptimizedClosures.FSharpFunc<_, _, _>.Adapt folder) (Single state) tree state

/// <summary>
/// Returns a tree that contains one item only.
/// </summary>
/// <param name="value">The input item.</param>
/// <returns>The result tree of one item.</returns>
let singleton value = Single value

/// <summary>
/// Returns a new tree that excludes the first <c>count</c> items.
/// </summary>
/// <param name="count">The number of items to skip.</param>
/// <param name="tree">The input tree.</param>
/// <returns>A new tree that excludes the first <c>count</c> items.</returns>
let rec skip count tree =
    if count = 0 then tree
    else
        match viewV tree with
        | ValueNone ->
            raise (ArgumentException "The number of items to skip exceeds the number of items in the collection.")
        | ValueSome (_, rest) ->
            skip (count - 1) rest

/// <summary>
/// Bypasses elements in a tree while the givefn predicate returns true, and then returns the remaining elements in a new tree.
/// </summary>
/// <param name="predicate">A function that evaluates each item to a boolean value.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The created sub tree.</returns>
let rec skipWhile predicate tree =
    match viewV tree with
    | ValueNone -> tree
    | ValueSome (item, rest) ->
        if predicate item then
            skipWhile predicate rest
        else
            rest

/// <summary>
/// Sorts the elements of the tree.
/// </summary>
/// <remarks>This is not a stable sort, for a stable sort consider using <c>Seq.sort</c>.</remarks>
/// <param name="tree">The input tree.</param>
/// <returns>The sorted tree.</returns>
let sort tree =
    tree
    |> toArray
    |> Array.sort
    |> ofArray
    
/// <summary>
/// Sorts the elements of the tree using the given projection for the keys.
/// </summary>
/// <remarks>This is not a stable sort, for a stable sort consider using <c>Seq.sortBy</c>.</remarks>
/// <param name="projection">Transforms each tree element into an item to be compared.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The sorted tree.</returns>
let sortBy projection tree =
    tree
    |> toArray
    |> Array.sortBy projection
    |> ofArray
    
/// <summary>
/// Sorts the elements of a tree using the given comparison function to determine the order.
/// </summary>
/// <remarks>This is not a stable sort, for a stable sort consider using <c>Seq.sortWith</c>.</remarks>
/// <param name="comparer">The function to compare elements.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The sorted tree.</returns>
let sortWith comparer tree =
    tree
    |> toArray
    |> Array.sortWith comparer
    |> ofArray

/// <summary>
/// Splits a tree into two trees, at the given index.
/// </summary>
/// <param name="index">The index at which the tree is split.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The two split trees.</returns>
/// <exception cref="System.InvalidOperationException">Thrown when split index exceeds the number of elements in the tree.</exception>
let splitAt index tree =
    let rec go index before tree =
        if index = 0 then before, tree
        else
            match viewV tree with
            | ValueNone ->
                invalidOp "Split index exceeds the number if items in the collection."
            | ValueSome (item, rest) ->
                go (index - 1) (insertRight item before) rest
        
    go index empty tree

/// <summary>
/// Sorts the elements of a tree in descending order.
/// </summary>
/// <remarks>This is not a stable sort, for a stable sort consider using <c>Seq.sortDescending</c>.</remarks>
/// <param name="tree">The input tree.</param>
/// <returns>The sorted tree.</returns>
let sortDescending tree =
    sortWith (fun a b -> compare b a) tree
    
/// <summary>
/// Sorts the elements of a tree in descending order using the keys from the given projection function..
/// </summary>
/// <remarks>This is not a stable sort, for a stable sort consider using <c>Seq.sortDescending</c>.</remarks>
/// <param name="projection">A function that maps elements to the keys they should be compared by.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The sorted tree.</returns>
let sortByDescending projection tree =
    sortWith (fun a b -> compare (projection b) (projection a)) tree
    
/// <summary>
/// Returns the sum of the elements in the tree.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The resulting sum.</returns>
let inline sum (tree: 't FingerTree) : 't when 't: (static member (+): 't * 't -> 't) and 't: (static member Zero: 't) =
    tree
    |> fold (fun total item -> total + item) 't.Zero

/// <summary>
/// Returns the sum of the results generated by applying the function to each element in the tree.
/// </summary>
/// <param name="projection">The function to transform each element into the type to be summ ed.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The resulting sum.</returns>
let inline sumBy (projection: 't -> 'u) (tree: 't FingerTree) : 'u when 'u: (static member (+): 'u * 'u -> 'u) and 'u: (static member Zero: 'u) =
    tree
    |> fold (fun total item -> total + (projection item)) 'u.Zero

/// <summary>
/// Returns the first <c>count</c> items from the given tree.
/// </summary>
/// <param name="count">The number of items to return.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The first <c>count</c> items from the given tree.</returns>
let take count tree =
    let rec go count result tree =
        if count = 0 then result
        else
            match viewV tree with
            | ValueNone ->
                invalidOp "The number of items to take exceeds the number of items in the collection."
            | ValueSome (item, rest) ->
                go (count - 1) (insertRight item result) rest      
    
    go count empty tree
              
/// <summary>
/// Returns all the elements that the predicate returned true on that occur before the first time it returned false.
/// </summary>
/// <param name="predicate">The function to evaluate each item to a boolean value.</param>
/// <param name="tree">The input tree.</param>
/// <returns>All the elements that the predicate returned true on that occur before the first time it returned false.</returns>
let takeWhile predicate tree =
    let rec go condition result tree =
        match viewV tree with
        | ValueNone -> result
        | ValueSome (item, rest) ->
            if condition item then
                go condition (insertRight item result) rest
            else
                result
    
    go predicate empty tree

/// <summary>
/// Returns a tree that contains all but the first element of the given tree.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>A tree that contains all but the first element of the given tree.</returns>
let tail tree = removeLeft tree

/// <summary>
/// Converts the tree into a list.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>A list with all the items contained by the tree and in the same order.</returns>
let toList tree =
    foldBack (fun item list -> item :: list) tree []

/// <summary>
/// Returns the transpose of the given sequence of trees.
/// </summary>
/// <param name="trees"></param>
/// <returns>The transposed array.</returns>
/// <exception cref="System.ArgumentException">Thrown when the input trees differ in length.</exception>
let transpose (trees: #seq<'t FingerTree>) =
    let trees = Seq.toArray trees
    let results = Array.replicate trees.Length empty
    
    let rec processTree (results: FingerTree<'t>[]) tree index =
        match viewV tree with
        | ValueNone -> ()
        | ValueSome (item, rest) ->
            results[index] <- insertRight item results[index]
            processTree results rest (index + 1)
    
    for tree in trees do
        processTree results tree 0
    
    ofArray trees

/// <summary>
/// Returns at most <c>count</c> items from the tree.
/// </summary>
/// <param name="count">The maximum number of items to return.</param>
/// <param name="tree">The input tree.</param>
/// <returns>At most <c>count</c> items from the tree.</returns>
let rec truncate count tree =
    let rec go result count =
        if count = 0 then result
        else
            match viewV tree with
            | ValueNone -> result
            | ValueSome (x, xs) ->
                go (insertRight x result) (count - 1)
        
    go empty count

/// <summary>
/// Returns the first item for which the predicate returns true or <c>None</c>.
/// </summary>
/// <param name="predicate">The condition to apply to each element.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The first item for which the predicate returns true or <c>None</c>.</returns>
let rec tryFind predicate tree =
    match viewV tree with
    | ValueNone -> None
    | ValueSome (item, rest) ->
        if predicate item then
            Some item
        else
            tryFind predicate rest
/// <summary>
/// Returns the first item for which the predicate returns true or <c>ValueNone</c>.
/// </summary>
/// <param name="predicate">The condition to apply to each element.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The first item for which the predicate returns true or <c>ValueNone</c>.</returns>
let rec tryFindV predicate tree =
    match viewV tree with
    | ValueNone -> ValueNone
    | ValueSome (item, rest) ->
        if predicate item then
            ValueSome item
        else
            tryFindV predicate rest
            
/// <summary>
/// Returns the last item for which the predicate returns true or <c>None</c>.
/// </summary>
/// <param name="predicate">The condition to apply to each element.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The last item for which the predicate returns true or <c>None</c>.</returns>
let rec tryFindBack predicate tree =
    match viewRevV tree with
    | ValueNone -> None
    | ValueSome (item, rest) ->
        if predicate item then
            Some item
        else
            tryFindBack predicate rest
/// <summary>
/// Returns the last item for which the predicate returns true or <c>ValueNone</c>.
/// </summary>
/// <param name="predicate">The condition to apply to each element.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The last item for which the predicate returns true or <c>ValueNone</c>.</returns>
let rec tryFindBackV predicate tree =
    match viewRevV tree with
    | ValueNone -> ValueNone
    | ValueSome (item, rest) ->
        if predicate item then
            ValueSome item
        else
            tryFindBackV predicate rest

/// <summary>
/// Returns the index of the first item for which the predicate returns true or <c>None</c>. 
/// </summary>
/// <param name="predicate">The predicate to evaluate each item with.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The index of the first item for which the predicate returns true or <c>None</c>.</returns>
let tryFindIndex predicate tree =
    let rec go predicate index tree =
        match viewV tree with
        | ValueNone -> None
        | ValueSome (item, rest) ->
            if predicate item then Some index
            else
                go predicate (index + 1) rest
    
    go predicate 0 tree

/// <summary>
/// Returns the index of the first item for which the predicate returns true or <c>ValueNone</c>. 
/// </summary>
/// <param name="predicate">The predicate to evaluate each item with.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The index of the first item for which the predicate returns true or <c>ValueNone</c>.</returns>
let tryFindIndexV predicate tree =
    let rec go predicate index tree =
        match viewV tree with
        | ValueNone -> ValueNone
        | ValueSome (item, rest) ->
            if predicate item then ValueSome index
            else
                go predicate (index + 1) rest
    
    go predicate 0 tree

/// <summary>
/// Attempts to fetch the item at the specified index.
/// This function is slow and requires O(index) time to find the item.
/// </summary>
/// <param name="index">The index of the item to fetch.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The specified item or <c>None</c> if the index was out of bounds.</returns>
let rec tryItem index tree =
    match tree with
    | Blank -> None
    | Single x -> if index = 0 then Some x else None
        
    | Deep(left, _, _) ->
        match left with
        | One a when index = 0 -> Some a
        
        | Two(a, _) when index = 0 -> Some a
        | Two(_, b) when index = 1 -> Some b
        
        | Three(a, _, _) when index = 0 -> Some a
        | Three(_, b, _) when index = 1 -> Some b
        | Three(_, _, c) when index = 2 -> Some c
        
        | Four(a, _, _, _) when index = 0 -> Some a
        | Four(_, b, _, _) when index = 1 -> Some b
        | Four(_, _, c, _) when index = 2 -> Some c
        | Four(_, _, _, d) when index = 3 -> Some d
        
        | _ ->
            match leftV tree with
            | ValueNone -> None
            | ValueSome value ->
                if index = 0 then Some value else tryItem (index - 1) (removeLeft tree)
/// <summary>
/// Attempts to fetch the item at the specified index.
/// This function is slow and requires O(index) time to find the item.
/// </summary>
/// <param name="index">The index of the item to fetch.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The specified item or <c>ValueNone</c> if the index was out of bounds.</returns>
let rec tryItemV index tree =
    match tree with
    | Blank -> ValueNone
    | Single x -> if index = 0 then ValueSome x else ValueNone
        
    | Deep(left, _, _) ->
        match left with
        | One a when index = 0 -> ValueSome a
        
        | Two(a, _) when index = 0 -> ValueSome a
        | Two(_, b) when index = 1 -> ValueSome b
        
        | Three(a, _, _) when index = 0 -> ValueSome a
        | Three(_, b, _) when index = 1 -> ValueSome b
        | Three(_, _, c) when index = 2 -> ValueSome c
        
        | Four(a, _, _, _) when index = 0 -> ValueSome a
        | Four(_, b, _, _) when index = 1 -> ValueSome b
        | Four(_, _, c, _) when index = 2 -> ValueSome c
        | Four(_, _, _, d) when index = 3 -> ValueSome d
        
        | _ ->
            match leftV tree with
            | ValueNone -> ValueNone
            | ValueSome value ->
                if index = 0 then ValueSome value else tryItemV (index - 1) (removeLeft tree)

/// <summary>
/// Returns the index of the last item for which the predicate returns true or <c>None</c>. 
/// </summary>
/// <param name="predicate">The predicate to evaluate each item with.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The index of the last item for which the predicate returns true or <c>None</c>.</returns>
let tryFindIndexBack predicate tree =
    tree
    |> toArray
    |> Array.tryFindBack predicate

/// <summary>
/// Returns a tree that contains the elements generated by the computation.
/// The generator is called repeatedly the build the list until it returns <c>None</c>.
/// The given initial <c>state</c> argument is passed to the element generator.
/// </summary>
/// <param name="generator">A function that takes in the current state and returns option tuple of the next element of the tree and the next state value.</param>
/// <param name="state">The initial state value.</param>
/// <returns>The result tree.</returns>
let unfold generator state =
    let rec go result state =
        match generator state with
        | Some (next, newState) ->
            go (insertRight next result) newState
        | None -> result
    
    go empty state 
/// <summary>
/// Returns a tree that contains the elements generated by the computation.
/// The generator is called repeatedly the build the list until it returns <c>ValueNone</c>.
/// The given initial <c>state</c> argument is passed to the element generator.
/// </summary>
/// <param name="generator">A function that takes in the current state and returns option tuple of the next element of the tree and the next state value.</param>
/// <param name="state">The initial state value.</param>
/// <returns>The result tree.</returns>
let unfoldV generator state =
    let rec go result state =
        match generator state with
        | ValueSome (next, newState) ->
            go (insertRight next result) newState
        | ValueNone -> result
    
    go empty state

/// <summary>
/// Splits a tree of pairs into two trees.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The two trees.</returns>
let unzip tree =
    let rec go tree resultA resultB =
        match viewV tree with
        | ValueNone -> resultA, resultB
        | ValueSome((a, b), rest) ->
            go rest (insertRight a resultA) (insertRight b resultB)
        
    go tree empty empty

/// <summary>
/// Splits a tree of triples into three trees.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>The three trees.</returns>
let unzip3 tree =
    let rec go tree resultA resultB resultC =
        match viewV tree with
        | ValueNone -> resultA, resultB, resultC
        | ValueSome((a, b, c), rest) ->
            go rest (insertRight a resultA) (insertRight b resultB) (insertRight c resultC)
        
    go tree empty empty empty

/// <summary>
/// Returns a tree of elements for which the given predicate returned <c>true</c>.
/// </summary>
/// <param name="predicate">The function to check each element with.</param>
/// <param name="tree">The input tree.</param>
/// <returns>A tree of elements for which the given predicate returned <c>true</c>.</returns>
let where predicate tree = filter predicate tree

/// <summary>
/// Returns a tree of sliding windows containing elements from the input.
/// </summary>
/// <param name="windowSize">The number of elements in each window.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The result tree.</returns>
/// <exception cref="System.ArgumentException">Thrown when windowSize is not positive.</exception>
let windowed windowSize tree =
    if windowSize < 0 then
        invalidArg (nameof windowSize) "Window size must be greater than or equal to zero."
    
    let rec go windows tree =
        if isEmpty tree then windows
        else
            let size = length tree
            
            if windowSize > size then windows
            else
                let window = take windowSize tree
                go (insertRight window windows) (removeLeft tree)
    
    go empty tree

/// <summary>
/// Creates a new tree that contains pairs of the corresponding elements from the two trees.
/// </summary>
/// <param name="tree1">The first input tree.</param>
/// <param name="tree2">The second input tree.</param>
/// <returns>
/// A new tree that contains pairs of the corresponding elements from the two trees.
/// </returns>
let zip tree1 tree2 =
    let rec go current tree1 tree2 =
        match viewV tree1, viewV tree2 with
        | ValueNone, ValueNone -> current
        | ValueNone, _ | _, ValueNone ->
            invalidArgNotOfEqualLength "tree"
        | ValueSome (itemA, restA), ValueSome (itemB, restB) ->
            go (insertRight (itemA, itemB) current) restA restB
    
    go empty tree1 tree2

/// <summary>
/// Creates a new tree that contains triples of the corresponding elements from each of the three trees.
/// </summary>
/// <param name="tree1">The first input tree.</param>
/// <param name="tree2">The second input tree.</param>
/// <param name="tree3">The third input tree.</param>
/// <returns>
/// A new tree that contains triples of the corresponding elements from each of the three trees.
/// </returns>
let zip3 tree1 tree2 tree3 =
    let rec go current tree1 tree2 tree3 =
        match viewV tree1, viewV tree2, viewV tree3 with
        | ValueNone, ValueNone, ValueNone -> current
        | ValueNone, _, _ | _, ValueNone, _ | _, _, ValueNone ->
            invalidArgNotOfEqualLength "tree"
        | ValueSome (itemA, restA), ValueSome (itemB, restB), ValueSome (itemC, restC) ->
            go (insertRight (itemA, itemB, itemC) current) restA restB restC
    
    go empty tree1 tree2 tree3

/// <summary>
/// Returns a new tree with the item at the given index removed.
/// </summary>
/// <param name="index">The index of the item to removed.</param>
/// <param name="source">The input tree.</param>
/// <returns>The result tree.</returns>
let removeAt index source =
    source
    |> fold (fun (itemIndex, result) item ->
        if itemIndex <> index then
            (itemIndex + 1, insertRight item result)
        else
            (itemIndex + 1, result)
    ) (0, empty)

/// <summary>
/// Returns a new tree with the items in the given range removed.
/// </summary>
/// <param name="index">The index of the items to removed.</param>
/// <param name="count">The number of items to be removed.</param>
/// <param name="source">The input tree.</param>
/// <returns>The result tree.</returns>
let removeManyAt index count source =
    source
    |> fold (fun (itemIndex, result) item ->
        if itemIndex < index || itemIndex >= index + count then
            (itemIndex + 1, insertRight item result)
        else
            (itemIndex + 1, result)
    ) (0, empty)

/// <summary>
/// Returns a new finger tree with the item at the given index set to the new value.
/// </summary>
/// <param name="index">The index of the item to change.</param>
/// <param name="value">The value to change it to.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The resulting tree.</returns>
let updateAt index value tree =
    mapi (fun itemIndex existing -> if index = itemIndex then value else existing) tree

/// <summary>
/// Returns a new tree with a new item inserted before the given index.
/// </summary>
/// <param name="index">The index where the item should be inserted.</param>
/// <param name="value">The value to insert.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The result tree.</returns>
let insertAt index value tree =
    tree
    |> fold (fun (itemIndex, result) existing ->
        if itemIndex = index then
            (itemIndex + 1, insertRight existing (insertRight value result))
        else
            (itemIndex + 1, insertRight existing result)
        
    ) (0, empty)

/// <summary>
/// Returns a new tree with the new items inserted before the given index.
/// </summary>
/// <param name="index">The index where the items should be inserted.</param>
/// <param name="values">The values to insert.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The result tree.</returns>
let insertManyAt index (values: #seq<'t>) tree =
    tree
    |> fold (fun (itemIndex, result) existing ->
        if itemIndex = index then
            (itemIndex + 1, values |> Seq.fold (fun tree item -> insertRight item tree) result)
        else
            (itemIndex + 1, insertRight existing result)
        
    ) (0, empty)

/// <summary>
/// Returns a new tree shuffled in a random order with the specified <c>Random</c> instance.
/// </summary>
/// <param name="random">The <c>Random</c> instance.</param>
/// <param name="source">The input tree.</param>
/// <returns>The result tree.</returns>
let randomShuffleWith (random: Random) source =
    source
    |> toArray
    |> Array.randomShuffleWith random
    |> ofArray
/// <summary>
/// Returns a new tree shuffled in a random order.
/// </summary>
/// <param name="source">The input tree.</param>
/// <returns>The result tree.</returns>
let randomShuffle source = randomShuffleWith Random.Shared
/// <summary>
/// Returns a new tree shuffled in a random order using the specified <c>randomizer</c> function.
/// </summary>
/// <param name="randomizer">The randomizer function, which must return a float number from the [0.0..1.0) range.</param>
/// <param name="source">The input tree.</param>
/// <returns>The result tree.</returns>
let randomShuffleBy randomizer source =
    source
    |> toArray
    |> Array.randomShuffleBy randomizer
    |> ofArray

/// <summary>
/// Returns a randomly selected element from the given tree using the given <c>Random</c> instance.
/// </summary>
/// <param name="random">The <c>Random</c> instance.</param>
/// <param name="source">The input tree.</param>
/// <returns>A randomly selected element from the given tree using the given <c>Random</c> instance.</returns>
let randomChoiceWith random source =
    source
    |> toArray
    |> Array.randomChoiceWith random
/// <summary>
/// Returns a randomly selected element from the given tree.
/// </summary>
/// <param name="source">The input tree.</param>
/// <returns>A randomly selected element from the given tree.</returns>
let randomChoice source = randomChoiceWith Random.Shared source
/// <summary>
/// Returns a randomly selected element from the given tree using the given <c>randomizer</c> function.
/// </summary>
/// <param name="randomizer">A function returning values from the range [0.0..1.0).</param>
/// <param name="source">The input tree.</param>
/// <returns>A randomly selected element from the given tree using the given <c>randomizer</c> function.</returns>
let randomChoiceBy (randomizer: unit -> float) source =
    source
    |> toArray
    |> Array.randomChoiceBy randomizer

/// <summary>
/// Returns a tree of randomly selected elements from the given tree using the given <c>Random</c> instance.
/// </summary>
/// <param name="random">The <c>Random</c> instance.</param>
/// <param name="count">The number of items to select.</param>
/// <param name="source">The input tree.</param>
/// <returns>A tree of randomly selected elements from the given tree using the given <c>Random</c> instance.</returns>
/// <exception cref="System.ArgumentException">Thrown when count is less than 0.</exception>
let randomChoicesWith (random: Random) count source =
    ensureNonNegative (nameof count) count
    let items = toArray source
    let mutable result = empty
    
    for _ in 1..count do
        let index = random.Next(0, items.Length)
        result <- insertRight items[index] result
    
    result
/// <summary>
/// Returns a tree of randomly selected elements from the given tree.
/// </summary>
/// <param name="count">The number of items to select.</param>
/// <param name="source">The input tree.</param>
/// <returns>A tree of randomly selected elements from the given tree.</returns>
/// <exception cref="System.ArgumentException">Thrown when count is less than 0.</exception>
let randomChoices count source =
    ensureNonNegative (nameof count) count
    randomChoicesWith Random.Shared count source
    
/// <summary>
/// Returns a tree of randomly selected elements from the given tree using the given <c>randomizer</c> function.
/// </summary>
/// <param name="randomizer">A function returning values from the range [0.0..1.0).</param>
/// <param name="count">The number of items to select.</param>
/// <param name="source">The input tree.</param>
/// <returns>A tree of randomly selected elements from the given tree using the given <c>randomizer</c> function.</returns>
/// <exception cref="System.ArgumentException">Thrown when count is less than 0.</exception>
let randomChoicesBy (randomizer: unit -> float) count source =
    ensureNonNegative (nameof count) count
    
    let items = toArray source
    let mutable result = empty
    
    for _ in 1..count do
        let index = int (randomizer() * float items.Length)
        result <- insertRight items[index] result

    result

/// <summary>
/// Returns a random sample of elements from the given tree, where each element can only be selected once, using the given <c>Random</c> instance.
/// </summary>
/// <param name="random">The <c>Random</c> instance to use.</param>
/// <param name="count">The number of elements to return.</param>
/// <param name="source">The input tree.</param>
/// <returns>A tree of randomly selected elements from the input tree.</returns>
let randomSampleWith (random: Random) count source =
    let items = toArray source
    
    ensureNonNegative (nameof count) count
    if count > items.Length then
        invalidArg (nameof count) $"Count may not be greater than the number of items in the collection."
    
    let seen = HashSet()
    let mutable result = empty
    
    for _ in 1..count do
        let mutable index = random.Next(0, items.Length)
        
        while seen.Contains(index) do
            index <- random.Next(0, items.Length)

        result <- insertRight items[index] result
        seen.Add(index) |> ignore
    
    empty
/// <summary>
/// Returns a random sample of elements from the given tree, where each element can only be selected once.
/// </summary>
/// <param name="count">The number of elements to return.</param>
/// <param name="source">The input tree.</param>
/// <returns>A tree of randomly selected elements from the input tree.</returns>
let randomSample count source = randomSampleWith Random.Shared count source
/// <summary>
/// Returns a random sample of elements from the given tree, where each element can only be selected once, using the given <c>randomizer</c> function.
/// </summary>
/// <param name="randomizer">A function returning values in the range [0.0..1.0).</param>
/// <param name="count">The number of elements to return.</param>
/// <param name="source">The input tree.</param>
/// <returns>A tree of randomly selected elements from the input tree.</returns>
let randomSampleBy randomizer count source =
    let items = toArray source
    
    ensureNonNegative (nameof count) count
    if count > items.Length then
        invalidArg (nameof count) $"Count may not be greater than the number of items in the collection."
    
    let seen = HashSet()
    let mutable result = empty
    
    for _ in 1..count do
        let mutable index = int (randomizer() * float items.Length)

        while seen.Contains(index) do
            index <- int (randomizer() * float items.Length)

        result <- insertRight items[index] result
        seen.Add(index) |> ignore

    empty

// Generic

/// <summary>
/// Returns a tree that repeats the given values <c>count</c> times.
/// </summary>
/// <param name="count">The number of times to repeat the values.</param>
/// <param name="tree">The values to repeat.</param>
/// <returns>A tree that repeats the given values <c>count</c> times.</returns>
/// <exception cref="System.ArgumentException"><c>count</c> is less than zero.</exception>
let repeat count (tree: FingerTree<'t>) =
    ensureNonNegative (nameof count) count
    
    let rec go result times =
        if times = 0 then result
        else go (append result tree) (times - 1)

    go empty count

/// <summary>
/// Creates a tree of trees with the specified number of inner and outer items and initialized with the given function.
/// </summary>
/// <param name="inner">The number of items in each inner tree.</param>
/// <param name="outer">The number of lists in the outer tree.</param>
/// <param name="initializer">The initializer function.</param>
/// <exception cref="System.ArgumentException"><c>inner</c> was less than zero.</exception>
/// <exception cref="System.ArgumentException"><c>outer</c> was less than zero.</exception>
let table inner outer initializer =
    ensureNonNegative (nameof inner) inner
    ensureNonNegative (nameof outer) outer
    
    init outer (fun _ -> init inner initializer)

/// <summary>
/// Applies the mapping function to each element, threading an accumulator through the computation, building a new tree containing pairs of the state and the mapped item.
/// </summary>
/// <param name="mapping">The mapping to apply.</param>
/// <param name="state">The initial starting state.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The resulting tree.</returns>
let mapScan mapping (state: 'state) tree =
    tree
    |> scan (fun (state, _) -> mapping state) (state, Unchecked.defaultof<'t>)

/// <summary>
/// Applies the mapping function to each element of the tree in reverse order, threading an accumulator through the computation, building a new tree containing pairs of the state and the mapped item.
/// </summary>
/// <param name="mapping">The mapping to apply.</param>
/// <param name="state">The initial starting state.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The resulting tree.</returns>
let mapScanBack mapping (state: 'state) tree =
    scanBack (fun item (state, _) ->
        mapping item state
    ) tree (state, Unchecked.defaultof<'t>)

/// <summary>
/// Converts an untyped <c>IEnumerator</c> in a list of objects.
/// </summary>
/// <param name="enumerator">The enumerator to convert.</param>
/// <returns>The resulting list.</returns>
/// <exception cref="System.NullArgumentException"><c>enumerator</c> was null.</exception>
let fromUntypedEnumerator (enumerator: IEnumerator) =
    ensureNotNull (nameof enumerator) enumerator
 
    let rec go result =
        if enumerator.MoveNext() then
            go (insertRight enumerator.Current result)
        else
            result
            
    go empty

/// <summary>
/// Converts an <c>IEnumerator&lt;'t&gt;</c> to a list of 't.
/// </summary>
/// <param name="enumerator">The enumerator to convert.</param>
/// <returns>The resulting list.</returns>
/// <exception cref="System.NullArgumentException"><c>enumerator</c> was null.</exception>
let fromEnumerator (enumerator: IEnumerator<'t>) =
    ensureNotNull (nameof enumerator) enumerator
    
    let rec go result =
        if enumerator.MoveNext() then
            go (insertRight enumerator.Current result)
        else
            result
            
    go empty

/// <summary>
/// Converts an object that acts like a collection (that is, has <c>Count</c> and <c>Item</c> defined) to a list.
/// </summary>
/// <param name="collection">The collection to convert.</param>
/// <returns>The resulting list.</returns>
let inline fromCollection< ^c, ^t when ^c : (member Count: int) and ^c : (member Item: int -> ^t) > (collection: ^c) =
    init collection.Count collection.Item

/// <summary>
/// Replaces all instances of <c>before</c> in the list with <c>after</c>.
/// </summary>
/// <param name="before">The value to replace.</param>
/// <param name="after">The value to replace with.</param>
/// <param name="list">The input list.</param>
/// <returns>The resulting list.</returns>
let replace before after list =
    list
    |> map (fun value -> if value = before then after else value)

/// <summary>
/// Applies a function to each element and its index, threading an accumulator through the computation.
/// </summary>
/// <param name="folder">The function to compute each state given the last.</param>
/// <param name="state">The initial starting state.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The resulting state.</returns>
let foldi folder (state: 'state) (tree: 't FingerTree) =    
    let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(folder)
    let rec go tree index result =
        match viewV tree with
        | ValueNone -> result
        | ValueSome (x, xs) ->
            go xs (index + 1) (f.Invoke(index, result, x))

    go tree 0 state

/// <summary>
/// Applies a function to each element and its index in reverse order, threading an accumulator through the computation.
/// </summary>
/// <param name="folder">The function to compute each state given the last.</param>
/// <param name="state">The initial starting state.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The resulting state.</returns>
let foldBacki folder (state: 'state) (tree: 't FingerTree) =
    let f = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt(folder)
    let rec go tree index result =
        match viewRevV tree with
        | ValueNone -> result
        | ValueSome (x, xs) ->
            go xs (index - 1) (f.Invoke(index, result, x))

    go tree (length tree - 1) state

/// <summary>
/// Applies the specified folding function to each element as long as the state is not <c>Done</c>.
/// </summary>
/// <param name="folder">The function to generate each state given the previous state.</param>
/// <param name="state">The initial state.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The resulting state.</returns>
let foldWhile folder state tree =
    let rec go (state: 'state) tree =
        match viewV tree with
        | ValueNone -> state
        | ValueSome (x, xs) ->
            let status, newState = folder state x

            match status with
            | Done -> newState
            | Continue ->
                go newState xs

    go state tree

/// <summary>
/// Applies the specified folding function to each element in reverse order as long as the state is not <c>Done</c>.
/// </summary>
/// <param name="folder">The function to generate each state given the previous state.</param>
/// <param name="state">The initial state.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The resulting state.</returns>
let foldBackWhile folder (tree: 't FingerTree) (state: 'state) =
    let rec go (state: 'state) tree =
        match viewRevV tree with
        | ValueNone -> state
        | ValueSome (x, xs) ->
            let status, newState = folder state x

            match status with
            | Done -> newState
            | Continue ->
                go newState xs

    go state tree

/// <summary>
/// Performs a standard fold unless the folding function returns <c>None</c>, in which case the overall function returns <c>None</c>.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The resulting state.</returns>
let tryFold folder state tree =
    let rec go (state: 'state) tree =
        match viewV tree with
        | ValueNone -> Some state
        | ValueSome (x, xs) ->
            let newState = folder state x

            match newState with
            | Some state ->
                go state xs
            | None -> None

    go state tree

/// <summary>
/// Performs a standard foldBack unless the folding function returns <c>None</c>, in which case the overall function returns <c>None</c>.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The resulting state.</returns>
let tryFoldBack folder tree state =
    let rec go (state: 'state) tree =
        match viewRevV tree with
        | ValueNone -> Some state
        | ValueSome (x, xs) ->
            let newState = folder state x

            match newState with
            | Some state ->
                go state xs
            | None -> None

    go state tree

/// <summary>
/// Combines fold and filter into a single function that threads the state object through the filtering process.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The resulting state.</returns>
let thresh folder (state: 'state) tree =
    let mutable state = state

    let filtered =
        tree
        |> filter (fun item ->
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
/// <param name="tree">The input tree.</param>
/// <returns>The resulting state.</returns>
let threshBack folder tree (state: 'state) =
    foldBack (fun item (result, state) ->
        let keep, state = folder item state

        if keep then
            (insertLeft item result, state)
        else
            (result, state)
    ) tree (empty, state)

/// <summary>
/// Combines fold and choose into a single function that threads the state object through the filtering process.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The resulting state.</returns>
let winnow folder (state: 'state) tree =
    fold (fun item (result, state) ->
        let value, state = folder item state
    
        match value with
        | Some newValue ->
            (insertRight newValue result), state
        | None ->
            result, state
    ) (empty, state) tree

/// <summary>
/// Combines fold and choose into a single function that threads the state object through the filtering process.
/// </summary>
/// <param name="folder">The function to generate each new state given the last.</param>
/// <param name="state">The initial state.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The resulting state.</returns>
let winnowBack folder tree (state: 'state) =
    foldBack (fun item (result, state) ->
        let value, state = folder item state
    
        match value with
        | Some newValue ->
            (insertLeft newValue result), state
        | None ->
            result, state
    ) tree (empty, state)

/// <summary>
/// Returns the first element for which the given predicate returns <c>true</c>.
/// </summary>
/// <param name="predicate">The predicate to evaluate each item with.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The first element for which the given predicate returns "true".</returns>
/// <exception cref="Functional.NoSuchItemException">The predicate did not evaluate to true for any items.</exception>
let findi predicate tree =
    let rec find index tree =
        match viewV tree with
        | ValueNone -> noSuchItem "An element matching the predicate was not found in the collection."
        | ValueSome(item, rest) ->
            if predicate index item then
                item
            else
                find (index + 1) rest

    find 0 tree

/// <summary>
/// Returns the last element for which the given predicate returns <c>true</c>.
/// </summary>
/// <param name="predicate">The predicate to evaluate each item with.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The first element for which the given predicate returns <c>true</c>.</returns>
/// <exception cref="Functional.NoSuchItemException">The predicate did not evaluate to true for any items.</exception>
let findBacki predicate tree =
    let rec find index tree =
        match viewRevV tree with
        | ValueNone -> noSuchItem "An element matching the predicate was not found in the collection."
        | ValueSome(item, rest) ->
            if predicate index item then
                item
            else
                find (index - 1) rest

    find (length tree - 1) tree

/// <summary>
/// Returns the first element for which the given predicate returns <c>true</c> or <c>None</c> if there is no such element.
/// </summary>
/// <param name="predicate">The predicate to check each item with.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The first element for which the given predicate returns <c>true</c> or <c>None</c> if there is no such element.</returns>
let tryFindi predicate tree =
    let rec go index tree =
        match viewV tree with
        | ValueNone -> None
        | ValueSome(item, rest) ->
            if predicate index item then
                Some item
            else
                go (index + 1) rest

    go 0 tree

/// <summary>
/// Returns the last element for which the given predicate returns <c>true</c> or <c>None</c> if there is no such element.
/// </summary>
/// <param name="predicate">The predicate to check each item with.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The last element for which the given predicate returns <c>true</c> or <c>None</c> if there is no such element.</returns>
let tryFindBacki predicate tree =
    let rec go index tree =
        match viewRevV tree with
        | ValueNone -> None
        | ValueSome(item, rest) ->
            if predicate index item then
                Some item
            else
                go (index - 1) rest

    go (length tree - 1) tree

/// <summary>
/// Returns the first element for which the given predicate returns <c>Some(x)</c>.
/// </summary>
/// <param name="chooser">The choice function to apply to each element.</param>
/// <param name="tree">The input tree.</param>
/// <exception cref="Functional.NoSuchItemException">The choice function returned <c>None</c> for all items.</exception>
/// <returns>The first element for which the given predicate returns <c>Some(x)</c>.</returns>
let picki chooser tree =
    let rec pick index tree =
        match viewV tree with
        | ValueNone -> noSuchItem "An element matching the predicate was not found in the collection."
        | ValueSome (item, rest) ->
            match chooser index item with
            | Some v -> v
            | None ->
                pick (index + 1) rest

    pick 0 tree

/// Returns the first element for which the given predicate returns Some x.
/// If there is no such element then None is returned instead.
/// <param name="list">The input list.</param>
/// <exception cref="System.NullArgumentException"><c>array</c> was null.</exception>
/// <returns>The first element for which the given predicate returns <c>Some(x)</c> or <c>None</c> if there is no such item.</returns>
let tryPicki predicate tree =
    let rec pick index tree =
        match viewV tree with
        | ValueNone -> None
        | ValueSome (item, rest) ->
            match predicate index item with
            | Some v -> Some v
            | None ->
                pick (index + 1) rest

    pick 0 tree

/// <summary>
/// Returns the concatenation of the arrays produced by applying the mapping function to each element and its index.
/// </summary>
/// <param name="mapping">The mapping function to apply.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The concatenation of the lists produced by applying the mapping function to each element and its index.</returns>
let collecti mapping tree =
    let rec go result index tree =
        match viewV tree with
        | ValueNone -> result
        | ValueSome (item, rest) ->
            mapping index item
            |> fold (fun result item -> insertRight item result) result
        
    go empty 0 tree

/// <summary>
/// Returns only those elements for which the predicate returned <c>true</c> when applied to said element and its index.
/// </summary>
/// <param name="predicate">The predicate to check pairings of elements and indexes with.</param>
/// <param name="tree">The input tree.</param>
/// <returns>Those elements for which the predicate returned <c>true</c> when applied to said element and its index.</returns>
let filteri predicate tree =
    tree
    |> fold (fun (index, result) item ->
        if predicate index item then
            (index + 1, insertRight item result)
        else
            (index + 1, result)
    ) (0, empty)
    
/// <summary>
/// Returns only those elements for which the predicate returned <c>Some(...)</c> when applied to said element and its index.
/// </summary>
/// <param name="predicate">The predicate to check pairings of elements and indexes with.</param>
/// <param name="tree">The input tree.</param>
/// <returns>Those elements for which the predicate returned <c>Some(...)</c> when applied to said element and its index.</returns>
let choosei predicate tree =
    tree
    |> fold (fun (index, result) item ->
        match predicate index item with
        | Some value ->
            (index + 1, insertRight value result)
        | None ->
            (index + 1, result)
    ) (0, empty)

/// <summary>
/// Removes all instances of the specified value from the list.
/// </summary>
/// <param name="value">The value to remove.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The input list minus all instances of the given item.</returns>
let without (value: 't) (tree: 't FingerTree) =
    tree
    |> filter ((<>) value)
/// <summary>
/// Removes all instances of the given values from the list.
/// </summary>
/// <param name="values">The values to remove.</param>
/// <param name="list">The input list.</param>
/// <returns>The input list minus all instances of the given items.</returns>
/// <exception cref="System.NullArgumentException"><c>values</c> was null.</exception>
let withoutMany (values: #seq<'t>) (list: 't FingerTree) =
    ensureNotNull (nameof values) values
    
    let items = Set.ofSeq values

    list
    |> filter (Set.contains @@ items >> not)

/// <summary>
/// Generates all possible insertions of a value into a tree.
/// </summary>
/// <param name="value">The value to insert.</param>
/// <param name="tree">The tree to insert into.</param>
/// <returns>A tree of trees containing all possible insertions of a value into a tree, where each subtree is the result of one possible insertion point.</returns>
/// <example>
/// Executing <c>insertions 4 (fingerTree { 1; 2; 3 })</c> would produce:
/// <code>
/// fingerTree {
///     fingerTree { 4; 1; 2; 3 }
///     fingerTree { 1; 4; 2; 3 }
///     fingerTree { 1; 2; 4; 3 }
///     fingerTree { 1; 2; 3; 4 }
/// }
/// </code>
/// </example>
let insertions value tree =
    let size = length tree + 1
    
    init size (fun insertIndex ->
        let rec go index result tree =
            if index = insertIndex then
                go (index + 1) (insertRight value result) tree
            else
                match viewV tree with
                | ValueNone -> result
                | ValueSome (x, xs) ->
                    go (index + 1) (insertRight x result) xs
            
        go 0 empty tree
    )
/// <summary>
/// Computes all possible permutations of the given values.
/// </summary>
/// <param name="tree">The input tree.</param>
/// <returns>All possible permutations of the given values.</returns>
let rec permutations tree =
    match viewV tree with
    | ValueNone ->
        singleton empty
    | ValueSome (head, tail) ->
        collect (insertions head) (permutations tail)

/// <summary>
/// Returns all pairs where both items are from unique indexes.
/// </summary>
/// <param name="list">The input list.</param>
/// <returns>All pairs where both items are from unique indexes.</returns>
let pairs list =
    fingerTree {
        let mutable outerIndex = 0
        
        for first in list do
            let mutable innerIndex = 0
            
            for second in list do
                if innerIndex <> outerIndex then
                    yield (first, second)
                
                innerIndex <- innerIndex + 1
            
            outerIndex <- outerIndex + 1
    }

/// <summary>
/// Splits the input list into multiple lists.
/// The provided `options` parameter determines what is done with the separating element.
/// </summary>
/// <param name="options">What to do with the separator.</param>
/// <param name="predicate">The predicate to determine if an element is a separator.</param>
/// <param name="list">The list to split.</param>
let splitByOptions options predicate (list: 't list) =
    fingerTree {
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
    }
/// <summary>
/// Splits the input list into multiple lists.
/// The separating element is not included as part of the results.
/// </summary>
/// <param name="predicate">The predicate to determine if an element is a separator.</param>
/// <param name="list">The list to split.</param>
let splitBy predicate (list: 't list) = splitByOptions DoNotIncludeSeparator predicate list

/// <summary>
/// Collapses a tree according to the specified rule.
/// All pairs which the rule returns <c>Some(x)</c> are replaced by <c>x</c>.
/// This happens recursively for any new pairs created by the merging of a previous pair.
/// </summary>
/// <param name="rule">The rule to apply to each pair of elements.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The resulting tree.</returns>
let collapse (rule: 't -> 't -> 't option) tree =
    match viewV tree with
    | ValueNone -> empty
    | ValueSome (head, tail) ->
        fingerTree {
            let mutable state = head
            let mutable list = tail
            
            while not list.IsEmpty do
                match viewV list with
                | ValueSome (head, tail) ->
                    match rule state head with
                    | Some nextState ->
                        state <- nextState
                        list <- tail
                    | None ->
                        yield state
                        state <- head
                        list <- tail
                    
                // This can't happen.
                | ValueNone -> ()
            
            yield state
        }

/// <summary>
/// Determines the number of elements for which the predicate returns true.
/// </summary>
/// <param name="predicate">The predicate to check each item with.</param>
/// <param name="tree">The input tree.</param>
/// <returns>The number of elements for which the predicate returns true.</returns>
let count predicate (tree: 't FingerTree) =
    let mutable count = 0
    
    for item in tree do
        if predicate item then
            count <- count + 1
    
    count
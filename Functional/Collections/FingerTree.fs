namespace rec Functional

open System
open System.Collections.Generic

[<AutoOpen>]
module private FingerTreeInternals =
    [<StructuredFormatDisplay "{AsString}">]
    type Node<'t> =
        | Node2 of 't * 't
        | Node3 of 't * 't * 't
    with
        member private this.AsString =
            match this with
            | Node2 (a, b) -> $"{a}, {b}"
            | Node3 (a, b, c) -> $"{a}, {b}, {c}"

        override this.ToString () = this.AsString

    [<StructuredFormatDisplay "{AsString}">]
    type Digit<'t> =
        | One of 't
        | Two of 't * 't
        | Three of 't * 't * 't
        | Four of 't * 't * 't * 't
    with
        member private this.AsString =
            match this with
            | One a -> if isNull (box a) then "" else a.ToString()
            | Two (a, b) -> $"{a}, {b}"
            | Three (a, b, c) -> $"{a}, {b}, {c}"
            | Four (a, b, c, d) -> $"{a}, {b}, {c}, {d}"
        member this.Item with get index =
            match this with
            | One a when index = 0 -> a
            
            | Two (a, _) when index = 0 -> a
            | Two (_, b) when index = 1 -> b
            
            | Three(a, _, _) when index = 0 -> a
            | Three(_, b, _) when index = 1 -> b
            | Three(_, _, c) when index = 2 -> c
            
            | Four(a, _, _, _) when index = 0 -> a
            | Four(_, b, _, _) when index = 1 -> b
            | Four(_, _, c, _) when index = 2 -> c
            | Four(_, _, _, d) when index = 3 -> d
            
            | _ -> raise (ArgumentOutOfRangeException(nameof index, "Index must be non negative and less than the size of the collection."))
        
        override this.ToString () = this.AsString

    module Node =
        let inline toDigit node =
            match node with
            | Node2 (a, b) -> Two(a, b)
            | Node3 (a, b, c) -> Three(a, b, c)

        let inline map mapping node =
            match node with
            | Node2 (a, b) -> Node2 (mapping a, mapping b)
            | Node3 (a, b, c) -> Node3 (mapping a, mapping b, mapping c)

    module Digit =
        let append value digit =
            match digit with
            | One a -> Two(a, value)
            | Two (a, b) -> Three(a, b, value)
            | Three (a, b, c) -> Four(a, b, c, value)

            // Just ignore it; this case should never be called.
            | Four _ -> digit
        let prepend value digit =
            match digit with
            | One b -> Two(value, b)
            | Two (b, c) -> Three(value, b, c)
            | Three (b, c, d) -> Four(value, b, c, d)

            // Just ignore it; this case should never be called.
            | Four _ -> digit

        let inline map mapping digit =
            match digit with
            | One a -> One (mapping a)
            | Two (a, b) -> Two (mapping a, mapping b)
            | Three (a, b, c) -> Three (mapping a, mapping b, mapping c)
            | Four (a, b, c, d) -> Four (mapping a, mapping b, mapping c, mapping d)
        let inline rev digit =
            match digit with
            | One _ -> digit
            | Two (a, b) -> Two (b, a)
            | Three (a, b, c) -> Three (c, b, a)
            | Four (a, b, c, d) -> Four (d, c, b, a)

/// An efficient functional data structure designed to be used in implementing other data structures.
/// Provides amortized O(1) access to the leaves of the tree.
[<StructuredFormatDisplay "{AsString}">]
[<CustomEquality; NoComparison>]
type FingerTree<'t> =
    private
    | Blank
    | Single of 't
    | Deep of leftValue:Digit<'t> * nestedTree:FingerTree<Node<'t>> * rightValue:Digit<'t>
with
    interface IEnumerable<'t> with
        member this.GetEnumerator () =
            (FingerTree.toSeq this :> IEnumerable<'t>).GetEnumerator()

        member this.GetEnumerator(): Collections.IEnumerator = 
            (this :> IEnumerable<'t>).GetEnumerator() :> Collections.IEnumerator
    interface IReadOnlyCollection<'t> with
        member this.Count = this.Size
    interface IReadOnlyList<'t> with
        member this.Item with get index =
            FingerTree.item index this

    member private this.AsString =
        match this with
        | Blank -> "()"
        | Single value -> $"({value})"
        | Deep (prefix, tree, suffix) -> $"({prefix}, {tree}, {suffix})"

    /// Determines whether or not this tree contains any elements.
    member this.IsEmpty =
        match this with
        | Blank -> true
        | Single _ -> false
        | Deep _ -> false

    /// The total number of leaves in the tree.
    member this.Size = FingerTree.size this
    
    member this.Item with get index =
        FingerTree.item index this

    override this.ToString () = this.AsString

    override this.Equals (other: obj): bool =
        match other with
        | :? FingerTree<'t> as other ->
            let rec compare a b =
                match FingerTree.view a with
                | EmptyView ->
                    match FingerTree.view b with
                    | EmptyView -> true
                    | View _ -> false

                | View (va, a) ->
                    match FingerTree.view b with
                    | EmptyView -> false
                    | View (vb, b) ->
                        if Unchecked.equals va vb then compare a b else false

            compare this other

        | _ -> false

    override this.GetHashCode () =
        Unchecked.hash (FingerTree.toList this)

/// A lazily produced list like view of a finger tree.
type View<'t> =
    | EmptyView
    | View of 't * FingerTree<'t>

[<RequireQualifiedAccess>]
module FingerTree =
    [<AutoOpen>]
    module private Internals =
        let inline promoteDigit digit =
            match digit with
            | One a -> Single a
            | Two (a, b) -> Deep(One a, Blank, One b)
            | Three (a, b, c) -> Deep (One a, Blank, Two (b, c))
            | Four (a, b, c, d) -> Deep (Two (a, b), Blank, Two (c, d))

    /// Determines whether or not this tree contains any elements.
    let isEmpty (tree: FingerTree<'t>) = tree.IsEmpty

    /// Lazily converts a finger tree into a list like view.
    let rec view<'t> (tree: FingerTree<'t>) : View<'t> =
        match tree with
        | Blank -> EmptyView
        | Single value -> View(value, Blank)
        
        | Deep(One value, middle, right) ->
            let rest =
                match view middle with
                | EmptyView -> promoteDigit right
                | View (node, rest) ->
                    Deep (Node.toDigit node, rest, right)
            
            View(value, rest)
        
        | Deep(Two (a, b), middle, right) ->
            View(a, Deep (One b, middle, right))
        
        | Deep(Three (a, b, c), middle, right) ->
            View(a, Deep (Two (b, c), middle, right))
        
        | Deep(Four (a, b, c, d), middle, right) ->
            View(a, Deep (Three (b, c, d), middle, right))
    /// Lazily converts a finger tree into a list like view that is in reverse order.
    let rec viewRev<'t> (tree: FingerTree<'t>) : View<'t> =
        match tree with
        | Blank -> EmptyView
        | Single value -> View(value, Blank)
        
        | Deep(left, middle, One value) ->
            let rest =
                match view middle with
                | EmptyView -> promoteDigit left
                | View (node, rest) ->
                    Deep (left, rest, Node.toDigit node)
            
            View(value, rest)
        
        | Deep(left, middle, Two (b, a)) ->
            View (a, Deep (left, middle, One b))
        
        | Deep(left, middle, Three (c, b, a)) ->
            View(a, Deep (left, middle, Two (c, b)))
        
        | Deep(left, middle, Four (d, c, b, a)) ->
            View(a, Deep (left, middle, Three (d, c, b)))

    /// An empty finger tree.
    let empty<'t> = Blank : FingerTree<'t>

    /// Attempts to return the leftmost leaf of the tree.
    let left (tree: FingerTree<'t>) =
        match tree with
        | Blank -> None
        | Single x -> Some x
        | Deep (One x, _, _) -> Some x
        | Deep (Two (x, _), _, _) -> Some x
        | Deep (Three (x, _, _), _, _) -> Some x
        | Deep (Four (x, _, _, _), _, _) -> Some x

    /// Attempts to return the rightmost leaf of the tree.
    let right (tree: FingerTree<'t>) =
        match tree with
        | Blank -> None
        | Single x -> Some x
        | Deep (_, _, One x) -> Some x
        | Deep (_, _, Two (_, x)) -> Some x
        | Deep (_, _, Three (_, _, x)) -> Some x
        | Deep (_, _, Four (_, _, _, x)) -> Some x

    /// Removes the leftmost leaf from the tree.
    let rec removeLeft<'t> (tree: FingerTree<'t>): FingerTree<'t> =
        match tree with
        | Blank -> Blank
        | Single _ -> Blank
        
        | Deep(One _, middle, right) ->
            match view middle with
            | EmptyView -> promoteDigit right
            | View (node, rest) ->
                Deep (Node.toDigit node, rest, right)

        | Deep(Two (_, b), middle, right) ->
            Deep (One b, middle, right)
        
        | Deep(Three (_, b, c), middle, right) ->
            Deep (Two (b, c), middle, right)
        
        | Deep(Four (_, b, c, d), middle, right) ->
            Deep (Three (b, c, d), middle, right)
        
    /// Removes the rightmost leaf from the tree.
    let removeRight (tree: FingerTree<'t>) =
        match tree with
        | Blank -> Blank
        | Single _ -> Blank
        
        | Deep(left, middle, One _) ->
            match view middle with
            | EmptyView -> promoteDigit left
            | View (node, rest) ->
                Deep (left, rest, Node.toDigit node)
        
        | Deep(left, middle, Two (b, _)) ->
            Deep (left, middle, One b)
        
        | Deep(left, middle, Three (c, b, _)) ->
            Deep (left, middle, Two (c, b))
        
        | Deep(left, middle, Four (d, c, b, _)) ->
            Deep (left, middle, Three (d, c, b))

    /// Returns the leftmost leaf of the tree and a new tree that is missing that leaf.
    let popLeft (tree: FingerTree<'t>) =
        match tree with
        | Blank -> None, Blank
        | Single x -> Some x, Blank
        
        | Deep(One x, middle, right) ->
            let rest =
                match view middle with
                | EmptyView -> promoteDigit right
                | View (node, rest) ->
                    Deep (Node.toDigit node, rest, right)

            Some x, rest

        | Deep(Two (a, b), middle, right) ->
            Some a, Deep (One b, middle, right)
        
        | Deep(Three (a, b, c), middle, right) ->
            Some a, Deep (Two (b, c), middle, right)
        
        | Deep(Four (a, b, c, d), middle, right) ->
            Some a, Deep (Three (b, c, d), middle, right)
    /// Returns the rightmost leaf of the tree and a new tree that is missing that leaf.
    let popRight (tree: FingerTree<'t>) =
        match tree with
        | Blank -> None, Blank
        | Single x -> Some x, Blank
        
        | Deep(left, middle, One x) ->
            let rest =
                match view middle with
                | EmptyView -> promoteDigit left
                | View (node, rest) ->
                    Deep (left, rest, Node.toDigit node)

            Some x, rest
        
        | Deep(left, middle, Two (b, a)) ->
            Some a, Deep (left, middle, One b)
        
        | Deep(left, middle, Three (c, b, a)) ->
            Some a, Deep (left, middle, Two (c, b))
        
        | Deep(left, middle, Four (d, c, b, a)) ->
            Some a, Deep (left, middle, Three (d, c, b))

    let rec item index tree =
        tree
        |> toSeq
        |> Seq.item index

    /// Appends the specified value to the front of the tree.
    let rec appendLeft<'t> (value: 't) (tree: FingerTree<'t>) : FingerTree<'t> =
        match tree with
        | Blank -> Single value
        | Single b -> Deep (One value, Blank, One b)

        | Deep (Four (b, c, d, e), middle, right) ->
            Deep (Two(value, b), appendLeft (Node3 (c, d, e)) middle, right)

        | Deep (left, middle, right) ->
            Deep (Digit.prepend value left, middle, right)
    /// Appends the specified value to the back of the tree.
    let rec appendRight<'t> (value: 't) (tree: FingerTree<'t>): FingerTree<'t> =
        match tree with
        | Blank -> Single value
        | Single a -> Deep (One a, Blank, One value)

        | Deep (left, middle, Four (a, b, c, d)) ->
            Deep (left, appendRight (Node3 (a, b, c)) middle, Two(d, value))

        | Deep (left, middle, right) ->
            Deep (left, middle, Digit.append value right)

    /// Determines if the given tree meets the required definition of shallowness.
    /// This is often used to swap between two different algorithms:
    /// One that is faster on small trees, and one that is better suited for big trees.
    let rec private isShallow<'t> (tree: FingerTree<'t>) (depth: int) (threshold: int): bool =
        match tree with
        | Blank -> true
        | Single _ -> true
        | Deep (_, Blank, _) -> true
        | Deep (_, middle, _) ->
            if depth + 1 > threshold then
                false
            else
                isShallow middle (depth + 1) threshold

    /// Applies a function to each element of the tree, threading an accumulator argument through the computation.
    /// Take the second argument, and apply the function to it and the first element of the tree.
    /// Then feed this result into the function along with the second element and so on.
    /// Return the final result.
    /// If the input function is f and the elements are i0...iN then computes f (... (f s i0) i1 ...) iN.
    let fold (folder: 'state -> 't -> 'state) (state: 'state) (tree: FingerTree<'t>) : 'state =
        let rec fold state rest =
            match view rest with
            | EmptyView -> state
            | View (value, rest) ->
                fold (folder state value) rest

        fold state tree

    /// Applies a function to each element of the tree, starting from the end, threading an accumulator argument through the computation.
    /// If the input function is f and the elements are i0...iN then computes f i0 (...(f iN s)).
    let foldBack (folder: 't -> 'state -> 'state) (tree: FingerTree<'t>) (state: 'state) =    
        let rec foldBack state rest =
            match viewRev rest with
            | EmptyView -> state
            | View (value, rest) ->
                foldBack (folder value state) rest

        foldBack state tree

    /// Merges the two trees into a single larger tree.
    let merge (left: FingerTree<'t>, right: FingerTree<'t>) =
        right
        |> fold (fun state item -> appendRight item state) left

    let rec private mapShallow<'t, 'u> (mapping: 't -> 'u) (tree: FingerTree<'t>): FingerTree<'u> =
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
            appendRight (mapping item) state
        ) Blank tree

    /// Creates a new finger tree where each element is the result of applying the specified mapping function to each corresponding element in the input tree.
    let map (mapping: 't -> 'u) (tree: FingerTree<'t>): FingerTree<'u> =
        if isShallow tree 0 5 then
            mapShallow mapping tree
        else
            mapDeep mapping tree

    let mapFold (mapping: 'state -> 't -> 'u * 'state) (state: 'state) (tree: FingerTree<'t>) =
        fold (fun (result, state) item ->
            let item, newState = mapping state item
            (appendRight item result, newState)
        ) (Blank, state) tree
    
    /// Returns a new tree containing only those items for which the specified condition returned true.
    let filter (condition: 't -> bool) (tree: FingerTree<'t>) =
        fold (fun state item ->
            if condition item then
                appendRight item state
            else
                state
        ) Blank tree

    /// Returns a new tree containing only those items for which the specified condition returned Some(x).
    let choose (chooser: 't -> 'u option) (tree: FingerTree<'t>) =
        fold (fun state item ->
            match chooser item with
            | Some value -> appendRight value state
            | None -> state
        ) Blank tree

    let rec private revShallow<'t> (tree: FingerTree<'t>): FingerTree<'t> =
        match tree with
        | Blank | Single _ -> tree
        | Deep (left, middle, right) ->
            Deep (Digit.rev right, revShallow middle, Digit.rev left)
    let private revDeep (tree: FingerTree<'t>) =
        fold (fun state item ->
            appendLeft item state
        ) Blank tree

    /// Reverses the left to right order of the tree.
    let rev (tree: FingerTree<'t>) =
        if isShallow tree 0 5 then
            revShallow tree
        else
            revDeep tree

    let splitAt (condition: 't -> bool) (tree: FingerTree<'t>) =
        let rec splitAt taken tree =
            match view tree with
            | EmptyView -> taken, None, Blank
            | View (item, rest) ->
                if condition item then
                    taken, Some item, rest
                else
                    splitAt (appendRight item taken) rest

        splitAt Blank tree
    let splitBy (condition: 't -> bool) (tree: FingerTree<'t>) =
        let rec splitBy taken tree =
            match view tree with
            | EmptyView -> taken, tree
            | View (item, rest) ->
                if condition item then
                    splitBy (appendRight item taken) rest
                else
                    taken, rest

        splitBy Blank tree
    let partition (condition: 't -> bool) (tree: FingerTree<'t>) =
        let rec partition taken left tree =
            match view tree with
            | EmptyView -> taken, tree
            | View (item, rest) ->
                if condition item then
                    partition (appendRight item taken) left rest
                else
                    partition taken (appendRight item left) rest

        partition Blank Blank tree
            
    /// Applies the specified function to every element in the tree.
    let iter (action: 't -> unit) (tree: FingerTree<'t>) =
        let rec iter rest =
            match view rest with
            | EmptyView -> ()
            | View (value, rest) ->
                action value
                iter rest

        iter tree
    /// Applies the specified function to the pair of every element in the tree and the it's matching index.
    let iteri (action: int -> 't -> unit) (tree: FingerTree<'t>) =
        let rec iteri index rest =
            match view rest with
            | EmptyView -> ()
            | View (value, rest) ->
                action index value
                iteri (index + 1) rest

        iteri 0 tree

    let size (tree: FingerTree<'t>) = 
        let rec count total (tree: FingerTree<'t>) =
            match tree with
            | Blank -> 0
            | Single _ -> 1
            
            | Deep(One _, middle, right) ->
                let rest =
                    match view middle with
                    | EmptyView -> promoteDigit right
                    | View (node, rest) ->
                        Deep (Node.toDigit node, rest, right)
                
                count (total + 1) rest
            
            | Deep(Two (_, b), middle, right) ->
                let rest = Deep (One b, middle, right)
                count (total + 1) rest
            
            | Deep(Three (_, b, c), middle, right) ->
                let rest = Deep (Two (b, c), middle, right)
                count (total + 1) rest
            
            | Deep(Four (_, b, c, d), middle, right) ->
                let rest = Deep (Three (b, c, d), middle, right)
                count (total + 1) rest
            
        count 0 tree

    let indexed tree =
        tree
        |> mapFold (fun index item -> (index, item), (index + 1)) 0

    /// Creates a finger tree from the specified list.
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
                appendRight item tree
            ) Blank

    /// Creates a finger tree from the specified array.
    let ofArray array =
        match array with
        | [||] -> Blank
        | [| a |] -> Single a
        | [| a; b |] -> Deep(One a, Blank, One b)
        | [| a; b; c |] -> Deep(One a, Blank, Two (b, c))
        | [| a; b; c; d |] -> Deep(Two (a, b), Blank, Two (c, d))
        | _ ->
            array
            |> Array.fold (fun tree item ->
                appendRight item tree
            ) Blank

    /// Creates a finger tree from the specified sequence.
    let ofSeq (sequence: #seq<'t>) =
        sequence
        |> Seq.fold (fun tree item -> appendRight item tree) Blank

    /// Converts the finger tree into a list.
    let toList tree =
        foldBack (fun item list -> item :: list) tree []

    /// Converts the finger tree into an array.
    let toArray tree =
        // Is there a better way?
        let result = Array.zeroCreate (size tree)

        fold (fun index element ->
            result[index] <- element
            index + 1
        ) 0 tree
        |> ignore

        result

    /// Converts the finger tree into a sequence.
    let toSeq tree =
        Seq.unfold (fun rest ->
            match view rest with
            | View (value, rest) -> Some (value, rest)
            | EmptyView -> None
        ) tree
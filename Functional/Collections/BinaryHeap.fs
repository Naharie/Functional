namespace Functional

/// A min based binary heap.
[<StructuredFormatDisplay "{AsString}">]
type BinaryHeap<'t> =
    private
    | Blank
    | One of (int * 't)
    | Node of (int * 't) * BinaryHeap<'t>
    | Pair of (int * 't) * BinaryHeap<'t> * BinaryHeap<'t> * hasEmptySpace:bool
with
    member private this.AsString =
        let indent (text: string) =
            "    " + text.Replace("\n", "\n    ")

        match this with
        | Blank -> "()"
        | One value -> string value
        | Node (value, child) -> string value + "\r\n" + indent (string child)
        | Pair (value, left, right, _) ->
            string value + "\r\n" + indent (string left) + "\r\n" + indent (string right)

    /// True if there is room to insert another element into the tree without creating new branches.
    member this.HasEmptySpace =
        match this with
        | Blank | Node _ -> true
        | One _ -> false
        | Pair (_, _, _, hasEmptySpace) -> hasEmptySpace

    override this.ToString () = this.AsString

[<RequireQualifiedAccess>]
module BinaryHeap =
    let empty<'t> : BinaryHeap<'t> = Blank
    let singleton item = One item

    let isEmpty heap =
        match heap with
        | Blank -> true
        | _ -> false

    let rec insert ((priority, _) as pair) heap =
        match heap with
        | Blank -> One pair
        | One ((priority', _) as pair') ->
            if priority < priority' then
                Node (pair, One pair')
            else
                Node (pair', One pair)
        
        | Node ((priority', _) as pair', child) ->
            let a, b =
                if priority < priority' then
                    pair, pair'
                else
                    pair', pair

            if child.HasEmptySpace then
                Node (a, insert b child)
            else
                Pair (a, child, One b, true)

        | Pair ((priority', _) as pair', left, right, _) ->
            let a, b =
                if priority < priority' then
                    pair, pair'
                else
                    pair', pair

            if left.HasEmptySpace then
                let newLeft = insert b left
                Pair (a, newLeft, right, newLeft.HasEmptySpace || right.HasEmptySpace)
            elif right.HasEmptySpace then
                let newRight = insert b right
                Pair (a, left, newRight, left.HasEmptySpace || newRight.HasEmptySpace)
            else
                if priority < priority' then
                    Node (pair, heap)
                else
                    let left = insert pair left
                    Pair (pair', left, right, left.HasEmptySpace)
    let rec merge a b =
        match b with
        | Blank -> a
        | One ((priority', _) as pair')
        | Node ((priority', _) as pair', _)
        | Pair ((priority', _) as pair', _, _, _) ->
            match a with
            | Blank -> b
            | One pair -> insert pair b

            | Node ((priority, _) as pair, child) ->
                if priority < priority' then
                    Pair (pair, child, b, child.HasEmptySpace || b.HasEmptySpace)
                else
                    Pair (pair', b, child, child.HasEmptySpace || b.HasEmptySpace)

            | Pair ((priority, _) as pair, left, right, _) ->
                if priority < priority' then
                    if left.HasEmptySpace then
                        let left = merge left b
                        Pair (pair, left, right, left.HasEmptySpace || right.HasEmptySpace)
                    elif right.HasEmptySpace then
                        let right = merge right b
                        Pair (pair, left, right, left.HasEmptySpace || right.HasEmptySpace)
                    else
                        Node (pair, Pair (pair', left, right, left.HasEmptySpace || right.HasEmptySpace))
                else
                    merge b a

    let ofList pairs =
        pairs
        |> List.fold (fun heap item -> insert item heap) empty

    /// An unsafe version of min.
    /// Throws an exception if the heap is empty.
    /// Should only be used in cases where the heap can be guarunteed to not be empty.
    let min heap =
        match heap with
        | Blank -> invalidOp "Can not fetch the minimum element of an empty heap."
        | One (_, v)
        | Node ((_, v), _)
        | Pair ((_, v), _, _, _) -> v
    let tryMin heap =
        match heap with
        | Blank -> ValueNone
        | One (_, v)
        | Node ((_, v), _)
        | Pair ((_, v), _, _, _) ->
            ValueSome v
    let removeMin heap =
        match heap with
        | Blank | One _ -> Blank
        | Node (_, child) -> child
        | Pair (_, left, right, _) -> merge left right

    let popMin heap =
        tryMin heap, removeMin heap
namespace Functional

[<Struct>]
type HeapKind = MinHeap | MaxHeap

type private BinaryHeapInternal<'t> =
    private
    | Blank
    | One of item:(int * 't)
    | Node of item: (int * 't) * heap:BinaryHeapInternal<'t>
    | Pair of (int * 't) * heapLeft:BinaryHeapInternal<'t> * heapRight:BinaryHeapInternal<'t> * hasEmptySpace:bool
with
    /// True if there is room to insert another element into the tree without creating new branches.
    member this.HasEmptySpace =
        match this with
        | Blank | Node _ -> true
        | One _ -> false
        | Pair (_, _, _, hasEmptySpace) -> hasEmptySpace

    override this.ToString () =
        let indent (text: string) =
            "    " + text.Replace("\n", "\n    ")

        match this with
        | Blank -> "()"
        | One value -> string value
        | Node (value, child) -> string value + "\r\n" + indent (string child)
        | Pair (value, left, right, _) ->
            string value + "\r\n" + indent (string left) + "\r\n" + indent (string right)

/// A min/max binary heap.
[<StructuredFormatDisplay "{AsString}">]
type BinaryHeap<'t> = private {
    kind: HeapKind
    heap: BinaryHeapInternal<'t>
}
with
    /// <summary>
    /// The extreme that this heap sorts towards.
    /// </summary>
    member this.Kind = this.kind
    
    /// <summary>
    /// Whether the heap contains no elements.
    /// </summary>
    member this.IsEmpty =
        match this.heap with
        | Blank -> true
        | One _ | Node _ | Pair _ -> false
    
    /// <summary>
    /// Whether there is room to insert another element into the tree without creating new branches.
    /// </summary>
    member this.HasEmptySpace = this.heap.HasEmptySpace
    
    member private this.AsString = this.heap.ToString()
    
    override this.ToString() = this.heap.ToString()

[<RequireQualifiedAccess>]
module BinaryHeap =
    /// <summary>
    /// An empty binary heap that sorts towards the specified extreme.
    /// </summary>
    /// <param name="kind">The extreme to sort towards.</param>
    /// <returns>An empty binary heap that sorts towards the specified extreme.</returns>
    let empty<'t> kind = { kind = kind; heap = Blank }
    
    /// An empty binary heap that sorts towards the minimum element.
    let emptyMinHeap<'t> = { kind = MinHeap; heap = Blank }
    /// An empty binary heap that sorts towards the maximum element.
    let emptyMaxHeap<'t> = { kind = MaxHeap; heap = Blank }
    
    /// <summary>
    /// Constructs a binary heap that contains the given element and sorts towards the specified extreme.
    /// </summary>
    /// <param name="kind">The extreme to sort towards.</param>
    /// <param name="item">The item to contain.</param>
    /// <returns>A binary heap that contains the given element and sorts towards the specified extreme.</returns>
    let singleton kind item = { kind = kind; heap = One item }

    /// <summary>
    /// Determines if the specified heap is empty.
    /// </summary>
    /// <param name="heap">The heap to check.</param>
    /// <returns>True if the heap is empty; false otherwise.</returns>
    let isEmpty (heap: BinaryHeap<'t>) = heap.IsEmpty

    let rec private insertInternal kind (priority, _ as pair) heap =
        let inline compare priorityA priorityB =
            match kind with
            | MinHeap -> priorityA < priorityB
            | MaxHeap -> priorityA > priorityB
        
        match heap with
        | Blank -> One pair
        | One (priority', _ as pair') ->
            if compare priority priority' then
                Node (pair, One pair')
            else
                Node (pair', One pair)
        
        | Node (priority', _ as pair', child) ->
            let a, b =
                if compare priority priority' then
                    pair, pair'
                else
                    pair', pair

            if child.HasEmptySpace then
                Node (a, insertInternal kind b child)
            else
                Pair (a, child, One b, true)

        | Pair (priority', _ as pair', left, right, _) ->
            let a, b =
                if compare priority priority' then
                    pair, pair'
                else
                    pair', pair

            if left.HasEmptySpace then
                let newLeft = insertInternal kind b left
                Pair (a, newLeft, right, newLeft.HasEmptySpace || right.HasEmptySpace)
            elif right.HasEmptySpace then
                let newRight = insertInternal kind b right
                Pair (a, left, newRight, left.HasEmptySpace || newRight.HasEmptySpace)
            else
                if compare priority priority' then
                    Node (pair, heap)
                else
                    let left = insertInternal kind pair left
                    Pair (pair', left, right, left.HasEmptySpace)
    
    /// <summary>
    /// Inserts the pairing of an element, and it's numeric weight into the given heap.
    /// </summary>
    /// <param name="pair">The pair to insert.</param>
    /// <param name="heap">The heap to insert into.</param>
    /// <returns>The updated heap.</returns>
    let insert pair heap =
        { heap with heap = insertInternal heap.kind pair heap.heap }
    
    let rec private mergeInternal kind a b =
        match b with
        | Blank -> a
        | One (priority', _ as pair')
        | Node (priority', _ as pair', _)
        | Pair (priority', _ as pair', _, _, _) ->
            match a with
            | Blank -> b
            | One pair -> insertInternal kind pair b

            | Node (priority, _ as pair, child) ->
                if priority < priority' then
                    Pair (pair, child, b, child.HasEmptySpace || b.HasEmptySpace)
                else
                    Pair (pair', b, child, child.HasEmptySpace || b.HasEmptySpace)

            | Pair (priority, _ as pair, left, right, _) ->
                if priority < priority' then
                    if left.HasEmptySpace then
                        let left = mergeInternal kind left b
                        Pair (pair, left, right, left.HasEmptySpace || right.HasEmptySpace)
                    elif right.HasEmptySpace then
                        let right = mergeInternal kind right b
                        Pair (pair, left, right, left.HasEmptySpace || right.HasEmptySpace)
                    else
                        Node (pair, Pair (pair', left, right, left.HasEmptySpace || right.HasEmptySpace))
                else
                    mergeInternal kind b a
    
    /// <summary>
    /// Attempts to merge two heaps, returning the merged heap or <c>None</c> if the heaps are of incompatible types.
    /// </summary>
    /// <param name="a">The first heap to merge.</param>
    /// <param name="b">The second heap to merge.</param>
    /// <returns>The merged heap or <c>None</c> if the heaps are of incompatible types.</returns>
    let tryMerge a b =
        if a.kind = b.kind then
            Some { kind = a.kind; heap = mergeInternal a.kind a.heap b.heap }
        else
            None
    
    /// <summary>
    /// Attempts to merge two heaps, returning the merged heap or <c>ValueNone</c> if the heaps are of incompatible types.
    /// </summary>
    /// <param name="a">The first heap to merge.</param>
    /// <param name="b">The second heap to merge.</param>
    /// <returns>The merged heap or <c>ValueNone</c> if the heaps are of incompatible types.</returns>
    let tryMergeV a b =
        if a.kind = b.kind then
            ValueSome { kind = a.kind; heap = mergeInternal a.kind a.heap b.heap }
        else
            ValueNone
    
    /// <summary>
    /// Computes the merger of two heaps.
    /// </summary>
    /// <param name="a">The first heap to merge.</param>
    /// <param name="b">The second heap to merge.</param>
    /// <returns>The merged heap.</returns>
    /// <exception cref="System.InvalidOperationException">The two heaps are of incompatible types.</exception>
    let merge a b =
        if a.kind <> b.kind then
            invalidOp "Can not merge two heaps that sort by different methods!"
        else
            { kind = a.kind; heap = mergeInternal a.kind a.heap b.heap }
    
    /// An unsafe version of min/max.
    /// Throws an exception if the heap is empty.
    /// Should only be used in cases where the heap can be guaranteed to not be empty.
    /// Returns the first item in order, whether that be the minimum or the maximum as defined by the heap kind.
    let extreme heap =
        match heap.heap with
        | Blank -> invalidOp "Can not fetch the extreme element of an empty heap."
        | One (_, v)
        | Node ((_, v), _)
        | Pair ((_, v), _, _, _) -> v
    let tryExtreme heap =
        match heap.heap with
        | Blank -> None
        | One (_, v)
        | Node ((_, v), _)
        | Pair ((_, v), _, _, _) ->
            Some v
    let removeExtreme heap =
        {
            kind = heap.kind
            heap =
                match heap.heap with
                | Blank | One _ -> Blank
                | Node (_, child) -> child
                | Pair (_, left, right, _) -> mergeInternal heap.kind left right
        }

    let popExtreme heap =
        tryExtreme heap, removeExtreme heap
        
    let ofList kind pairs =
        {
            kind = kind
            heap =
                pairs
                |> List.fold (fun heap item -> insertInternal kind item heap) Blank
        }

    let toList heap =
        let rec build items (heap: BinaryHeap<'t>) =
            if heap.IsEmpty then
                List.rev items
            else
                build (extreme heap :: items) (removeExtreme heap)
        
        build [] heap
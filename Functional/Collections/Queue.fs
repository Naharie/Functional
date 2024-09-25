namespace Functional

open System.Collections.Generic

/// An immutable queue with amortized O(1) access.
[<StructuredFormatDisplay "{AsString}">]
type queue<'t> =
    private | Queue of tree:FingerTree<'t>
with
    member private this.AsString =
        let (Queue tree) = this

        tree
        |> FingerTree.toList
        |> List.map (fun item ->
            match box item with
            | null -> "null"
            | item -> item.ToString()
        )
        |> String.concat "::"

    /// The number of items in the queue.
    member this.Count =
        let (Queue tree) = this
        tree.Size

    /// Returns a friendly representation of the queue as a string.
    override this.ToString () = this.AsString

    interface IEnumerable<'t> with
        member this.GetEnumerator () =
            let (Queue tree) = this
            (tree :> IEnumerable<'t>).GetEnumerator()

        member this.GetEnumerator(): System.Collections.IEnumerator =
            (this :> IEnumerable<'t>).GetEnumerator() :> System.Collections.IEnumerator
    interface IReadOnlyCollection<'t> with
        /// The number of items in the queue.
        member this.Count =
            let (Queue tree) = this
            tree.Size

type Queue<'t> = queue<'t>

module Queue =
    /// Returns an empty queue of the given type.
    let empty<'t> : 't queue = Queue FingerTree.empty

    /// Adds the specified item to the end of the queue.
    let enqueue (item: 't) (Queue tree) =
        Queue (FingerTree.appendLeft item tree)

    /// Attempts to remove the first item from the front of the queue.
    let rec dequeue (Queue tree) =
        let value, rest = FingerTree.popRight tree
        value, Queue rest

    /// Discards the first item from the front of the queue.
    let discard (Queue tree) =
        Queue (FingerTree.removeRight tree)

    /// Attempts to peek at the first item in the queue without removing it.
    let peek (Queue tree) =
        FingerTree.right tree

    /// Creates a queue from the specified list (first to last in order).
    let ofList (list: 't list) =
        Queue (FingerTree.ofList list)
    /// Creates a queue from the specified array (first to last in order).
    let ofArray (array: 't[]) =
        Queue (FingerTree.ofArray array)
    /// Creates a queue from the specified sequence (first to last in order).
    let ofSeq (seq: #seq<'t>) =
        Queue (FingerTree.ofSeq seq)

    /// Converts the queue to a list in the order first to last.
    let toList (Queue tree) =
        FingerTree.toList tree
    /// Converts the queue to an array in the order first to last.
    let toArray (Queue tree) =
        FingerTree.toArray tree
    /// Converts the queue to a sequence in the order first to last.
    let toSeq (Queue tree) =
        FingerTree.toSeq tree

    /// Returns the length of the queue.
    let length (Queue tree) =
        FingerTree.size tree

    /// Determines whether the queue is empty.
    let isEmpty (Queue tree) =
        FingerTree.isEmpty tree

    /// Appends the specified items to the queue.
    let enqueueAll (items: #seq<'t>) (queue: 't queue) =
        items
        |> Seq.fold (fun queue item ->
            enqueue item queue
        ) queue

    /// Applies the function to every item in the queue.
    let iter (action: 't -> unit) (Queue tree) =
        tree |> FingerTree.iter action
    /// Applies the function to the pair of every item in the queue and its index.
    let iteri (action: int -> 't -> unit) (Queue tree) =
        tree |> FingerTree.iteri action
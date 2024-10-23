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
        tree.Length

    /// <summary>
    /// Returns a friendly representation of the queue as a string.
    /// </summary>
    /// <returns>A friendly representation of the queue as a string.</returns>
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
            tree.Length

type Queue<'t> = queue<'t>

module Queue =
    /// An empty queue of the given type.
    let empty<'t> : 't queue = Queue FingerTree.empty

    /// <summary>
    /// Adds the specified item to the end of the queue.
    /// </summary>
    /// <param name="item">The item to enqueue.</param>
    /// <param name="queue">The queue to append to.</param>
    /// <returns>The resulting queue.</returns>
    let enqueue (item: 't) queue =
        let (Queue tree) = queue
        Queue (FingerTree.insertLeft item tree)

    /// <summary>
    /// Attempts to remove the first item from the front of the queue.
    /// </summary>
    /// <param name="queue">The queue to take the element from.</param>
    /// <returns>A pairing of the first element in the queue and the rest of the queue.</returns>
    let rec dequeue queue =
        let (Queue tree) = queue
        let value, rest = FingerTree.popRight tree
        value, Queue rest

    /// <summary>
    /// Discards the first item from the front of the queue.
    /// </summary>
    /// <param name="queue">The queue to remove the first element from.</param>
    /// <returns>The queue minus the first element.</returns>
    let discard queue =
        let (Queue tree) = queue
        Queue (FingerTree.removeRight tree)

    /// <summary>
    /// Attempts to peek at the first item in the queue without removing it.
    /// </summary>
    /// <param name="queue">The queue to peek at.</param>
    /// <returns></returns>
    let peek queue =
        let (Queue tree) = queue
        FingerTree.right tree

    /// <summary>
    /// Creates a queue from the specified list (last to first in order).
    /// </summary>
    /// <param name="list">The list to create the queue from.</param>
    /// <returns>The resulting queue.</returns>
    let ofList (list: 't list) =
        Queue (FingerTree.ofList list)
    /// <summary>
    /// Creates a queue from the specified array (last to first in order).
    /// </summary>
    /// <param name="array">The array to create the queue from.</param>
    /// <returns>The resulting queue.</returns>
    let ofArray (array: 't[]) =
        Queue (FingerTree.ofArray array)
    /// <summary>
    /// Creates a queue from the specified sequence (last to first in order).
    /// </summary>
    /// <param name="sequence">The sequence to create the queue from.</param>
    /// <returns>The resulting queue.</returns>
    let ofSeq (sequence: #seq<'t>) =
        Queue (FingerTree.ofSeq sequence)

    /// <summary>
    /// Converts the queue to a list in the order last to first.
    /// </summary>
    /// <param name="queue">The queue to convert.</param>
    /// <returns>The resulting list.</returns>
    let toList queue =
        let (Queue tree) = queue
        FingerTree.toList tree
    /// <summary>
    /// Converts the queue to an array in the order last to first.
    /// </summary>
    /// <param name="queue">The queue to convert.</param>
    /// <returns>The resulting array.</returns>
    let toArray queue =
        let (Queue tree) = queue
        FingerTree.toArray tree
    /// <summary>
    /// Converts the queue to a sequence in the order last to first.
    /// </summary>
    /// <param name="queue">The queue to convert.</param>
    /// <returns>The resulting sequence.</returns>
    let toSeq queue =
        let (Queue tree) = queue
        FingerTree.toSeq tree

    /// <summary>
    /// Returns the length of the queue.
    /// </summary>
    /// <param name="queue">The input queue.</param>
    /// <returns>The length of the queue.</returns>
    let length queue =
        let (Queue tree) = queue
        FingerTree.length tree

    /// <summary>
    /// Determines whether the queue is empty.
    /// </summary>
    /// <param name="queue">The input queue.</param>
    /// <returns>True if the queue is empty; false otherwise.</returns>
    let isEmpty queue =
        let (Queue tree) = queue
        FingerTree.isEmpty tree

    /// <summary>
    /// Appends all of the specified items to the queue.
    /// </summary>
    /// <param name="items">The items to enqueue.</param>
    /// <param name="queue">The queue to append to.</param>
    /// <returns>The resulting queue.</returns>
    let enqueueAll (items: #seq<'t>) (queue: 't queue) =
        items
        |> Seq.fold (fun queue item ->
            enqueue item queue
        ) queue

    /// <summary>
    /// Applies the function to every item in the queue.
    /// </summary>
    /// <param name="action">The action to apply.</param>
    /// <param name="queue">The queue containing the items to apply the function to.</param>
    let iter (action: 't -> unit) queue =
        let (Queue tree) = queue
        tree |> FingerTree.iterBack action
    /// <summary>
    /// Applies the function to the pair of every item in the queue and its index.
    /// </summary>
    /// <param name="action">The action to apply.</param>
    /// <param name="queue">The queue containing the items to apply the function to.</param>
    let iteri (action: int -> 't -> unit) queue =
        let (Queue tree) = queue
        tree |> FingerTree.iterBacki action
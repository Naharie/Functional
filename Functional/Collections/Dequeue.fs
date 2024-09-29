namespace Functional

open System.Collections.Generic
open Functional

/// An immutable dequeue with amortized O(1) access.
[<StructuredFormatDisplay "{AsString}">]
type dequeue<'t> =
    private | Dequeue of FingerTree<'t>
with
    member private this.AsString =
        let (Dequeue tree) = this

        tree
        |> FingerTree.toList
        |> List.map (fun item ->
            if isNull (box item) then "()" else item.ToString()
        )
        |> String.concat " :: "

    /// The number of items in the dequeue.
    member this.Count =
        let (Dequeue tree) = this
        tree.Length
    /// Returns a friendly representation of the dequeue as a string.
    override this.ToString () = this.AsString

    interface IEnumerable<'t> with
        member this.GetEnumerator () =
            let (Dequeue tree) = this
            (tree :> IEnumerable<'t>).GetEnumerator()

        member this.GetEnumerator(): System.Collections.IEnumerator =
            (this :> IEnumerable<'t>).GetEnumerator() :> System.Collections.IEnumerator
    interface IReadOnlyCollection<'t> with
        /// The number of items in the queue.
        member this.Count =
            let (Dequeue tree) = this
            tree.Length

type Dequeue<'t> = dequeue<'t>

module Dequeue =
    let inline private apply f dequeue =
        let (Dequeue tree) = dequeue
        f tree

    /// Returns an empty dequeue of the given type.
    let empty<'t> : 't dequeue = Dequeue FingerTree.empty

    /// Creates a dequeue from the specified list.
    let ofList (list: 't list) =
        Dequeue (FingerTree.ofList list)
    /// Creates a dequeue from the specified array.
    let ofArray (array: 't[]) =
        Dequeue (FingerTree.ofArray array)
    /// Creates a dequeue from the specified sequence.
    let ofSeq (seq: #seq<'t>) =
        Dequeue (FingerTree.ofSeq seq)

    /// Converts the queue to a list.
    let toList (dequeue: 't dequeue) =
        apply FingerTree.toList dequeue
    /// Converts the dequeue to an array.
    let toArray (dequeue: 't dequeue) =
        apply FingerTree.toArray dequeue
    /// Converts the dequeue to a sequence.
    let toSeq (dequeue: 't dequeue) =
        apply FingerTree.toSeq dequeue

    /// Returns the length of the dequeue.
    let length (dequeue: 't dequeue) =
        apply FingerTree.length dequeue

    /// Determines whether or not the dequeue is empty.
    let isEmpty (dequeue: 't dequeue) =
        apply FingerTree.isEmpty dequeue

    /// Appends an item to the left (front) of the dequeue.
    let appendLeft (item: 't) (dequeue: 't dequeue) =
        Dequeue (apply (FingerTree.insertLeft item) dequeue)
    /// Appends an item to the right (back) of the dequeue.
    let appendRight (item: 't) (dequeue: 't dequeue) =
        Dequeue (apply (FingerTree.insertRight item) dequeue)

    /// Attempts to peek at the leftmost item.
    let peekLeft (dequeue: 't dequeue) =
        apply FingerTree.left dequeue          
    /// Attempts to peek at the leftmost item.
    let peekRight (dequeue: 't dequeue) =
        apply FingerTree.right dequeue

    /// Removes the leftmost item from the dequeue and returns the item and the resulting dequeue.
    let popLeft (Dequeue tree) =
        let item, tree = FingerTree.popLeft tree
        item, Dequeue tree
    /// Removes the rightmost item from the dequeue and returns the item the resulting dequeue.
    let popRight (Dequeue tree) =
        let item, tree = FingerTree.popRight tree
        item, Dequeue tree
        
    /// Removes the leftmost item from the dequeue and returns the resulting dequeue.
    let discardLeft (dequeue: 't dequeue) =
        apply FingerTree.removeLeft dequeue
    /// Removes the rightmost item from the dequeue and returns the resulting dequeue.
    let discardRight (dequeue: 't dequeue) =
       apply FingerTree.removeRight dequeue

    let map mapping dequeue =
        let (Dequeue tree) = dequeue
        Dequeue (tree |> FingerTree.map mapping)

    /// Rotates the dequeue one step to the left.
    let rotateLeft (dequeue: 't dequeue) =
        let item, next = popLeft dequeue
        
        match item with
        | Some item ->
            appendRight item next
        // We only reach this case if the original dequeue was empty.
        | None -> empty
    /// Rotates the dequeue one step to the right.
    let rotateRight (dequeue: 't dequeue) =
        let item, next = popRight dequeue

        match item with
        | Some item ->
            appendLeft item next
        // We only reach this case if the original dequeue was empty.
        | None -> empty

    /// Rotates the dequeue by the specified number of steps.
    /// Positive numbers rotate to the right and negative numbers rotate to the left.
    let rotate (steps: int) (dequeue: 't dequeue) =
        let rec loop steps dequeue =
            if steps = 0 then dequeue
            elif steps < 0 then
                loop (steps + 1) (rotateLeft dequeue)
            else // steps > 0
                loop (steps - 1) (rotateRight dequeue)

        loop steps dequeue

    /// Applies the function to every item in the dequeue.
    let iter (action: 't -> unit) (dequeue: 't dequeue) =
        apply (FingerTree.iter action) dequeue
    /// Applies the function to the pair of every item in the dequeue and its index.
    let iteri (action: int -> 't -> unit) (dequeue: 't dequeue) =
        apply (FingerTree.iteri action) dequeue

    /// Reverse the dequeue.
    let reverse (dequeue: 't dequeue) =
        Dequeue (apply FingerTree.rev dequeue)
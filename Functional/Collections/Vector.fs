namespace rec Functional.Collections

open Functional
open System.Collections.Generic

type private leaf<'t> =
    | Leaf1 of 't
    | Leaf2 of 't * 't
    | Leaf3 of 't * 't * 't
    | Leaf4 of 't * 't * 't * 't

/// An immutable vector with O(1) append and O(log n) lookups and edits.
/// Lookups at the very front or very back are optimized and occur in O(1) time.
[<StructuredFormatDisplay "{AsString}">]
type vec<'t> =
    private
    | Leaf of leaf<'t>
    | Tree of size:int * leftLeaf:leaf<'t> * leftRoot:vec<'t> * rightRoot:vec<'t> * rightLeaf:leaf<'t>
with
    interface IEnumerable<'t> with
        member this.GetEnumerator () =
            (Vec.toSeq this :> IEnumerable<'t>).GetEnumerator()

        member this.GetEnumerator(): System.Collections.IEnumerator = 
            (this :> IEnumerable<'t>).GetEnumerator() :> System.Collections.IEnumerator
    interface IReadOnlyCollection<'t> with
        member this.Count = this.Length
        
    interface IReadOnlyList<'t> with
        member this.Item with get index = Vec.item index this
    
    /// The number of items contained by the vector.
    member this.Length =
        match this with
        | Leaf leaf -> Leaf.length leaf
        | Tree (size, _, _, _, _) -> size
    
    /// Whether this vector contains no items.
    member this.IsEmpty = this.Length = 0
    
    member this.Item with get index = Vec.item index this
    
    member private this.AsString =
        let items =
            this
            |> Vec.toSeq
            |> Seq.map (sprintf "%A")
            |> String.concat "; "
        
        $"vec {{ %s{items} }}"
        
    override this.ToString () = this.AsString

module private Leaf =
    let inline length leaf =
        match leaf with
        | Leaf1 _ -> 1
        | Leaf2 _ -> 2
        | Leaf3 _ -> 3
        | Leaf4 _ -> 4
    
    let inline item index leaf =
        let inline outOfRange() = indexOutOfRange "Index was outside the bounds of the vector."
        
        match leaf with
        | Leaf1 v ->
            if index = 0 then v else outOfRange()
        | Leaf2 (a, b) ->
            if index = 0 then a
            elif index = 1 then b
            else outOfRange()
        | Leaf3 (a, b, c) ->
            if index = 0 then a
            elif index = 1 then b
            elif index = 2 then c
            else outOfRange()
        | Leaf4 (a, b, c, d) ->
            if index = 0 then a
            elif index = 1 then b
            elif index = 2 then c
            elif index = 3 then d
            else outOfRange()
    let inline updateAt index value leaf =
        let inline outOfRange() = indexOutOfRange "Index was outside the bounds of the vector."
        
        match leaf with
        | Leaf1 _ ->
            if index = 0 then Leaf1 value else outOfRange()
        | Leaf2 (a, b) ->
            if index = 0 then Leaf2(value, b)
            elif index = 1 then Leaf2(a, value)
            else outOfRange()
        | Leaf3 (a, b, c) ->
            if index = 0 then Leaf3(value, b, c)
            elif index = 1 then Leaf3(a, value, c)
            elif index = 2 then Leaf3(a, b, value)
            else outOfRange()
        | Leaf4 (a, b, c, d) ->
            if index = 0 then Leaf4(value, b, c, d)
            elif index = 1 then Leaf4(a, value, c, d)
            elif index = 2 then Leaf4(a, b, value, d)
            elif index = 3 then Leaf4(a, b, c, value)
            else outOfRange()
            
    let toSeq leaf =
        match leaf with
        | Leaf1 a -> Seq.singleton a
        | Leaf2 (a, b) -> Seq.ofArray [| a; b |]
        | Leaf3 (a, b, c) -> Seq.ofArray [| a; b; c |]
        | Leaf4 (a, b, c, d) -> Seq.ofArray [| a; b; c; d |]
    
module Vec =
    // Core
    
    /// <summary>
    /// Returns the length of the specified vector.
    /// </summary>
    /// <param name="vec">The vector to fetch the length of.</param>
    /// <returns>The length of the specified vector.</returns>
    let length (vec: _ vec) = vec.Length
    
    /// <summary>Returns the item at the specified index.</summary>
    /// <exception cref="T:System.IndexOutOfRangeException">Thrown when the index is less than zero, greater than or eqaul to the size of the vector, or the vector is empty.</exception>
    /// <param name="index">The index of the item to fetch.</param>
    /// <param name="vec">The vector to fetch the item from.</param>
    /// <returns>The item at the specified index.</returns>
    let item index vec =
        let inline outOfRange() = indexOutOfRange "Index was outside the bounds of the vector."
        
        match vec with
        | Leaf leaf -> Leaf.item index leaf
            
        | Tree(size, leftLeaf, leftRoot, rightRoot, rightLeaf) ->
            if index < 0 || index >= size then outOfRange()
            else
                let leftLeafLength = Leaf.length leftLeaf
                let rightLeafLength = Leaf.length rightLeaf
                
                if index < leftLeafLength then
                    Leaf.item index leftLeaf
                elif index >= size - rightLeafLength then
                    Leaf.item (index - (size - rightLeafLength)) rightLeaf
                else
                    let index = index - leftLeafLength
                    let leftRootLength = length leftRoot
                    
                    if index < leftRootLength then
                        item index leftRoot
                    else
                        let index = index - leftRootLength
                        // Because of the optimized check above, we know that this index can't be asking for a value in the right leaf.
                        item index rightRoot
    
    /// <summary>
    /// Produces a sequence that enumerates all the values in the vector.
    /// </summary>
    /// <param name="vec">The vector to wrap as a sequence</param>
    /// <returns>The vector wrapped as a sequence.</returns>
    let toSeq vec =
        seq {
            match vec with
            | Leaf leaf ->
                yield! Leaf.toSeq leaf
            | Tree(_, leftLeaf, leftRoot, rightRoot, rightLeaf) ->
                yield! Leaf.toSeq leftLeaf
                yield! toSeq leftRoot
                yield! toSeq rightRoot
                yield! Leaf.toSeq rightLeaf
        }
    
    // Generic
    
    // Specific
    
    // Unsorted
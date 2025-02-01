namespace rec Functional.Collections
// The only recursive elements are VecElement referring to vec<'t> and vec<'t> referring to VecElement<'t>

open System
open System.Collections.Generic
open Functional
open Functional.Errors.CollectionErrors.NotEnoughItems

#nowarn "0044"

[<Obsolete>]
type VecElement<'t> =
    | VecNone
    | VecItem of 't
    | VecDeep of vec<'t>

/// An immutable vector with amortized O(1) append and O(log n) lookups and edits.
[<StructuredFormatDisplay "{AsString}">]
type vec<'t> =
    internal {
        elements: VecElement<'t>[]
        elementSize: int
        nextEmptySlot: int
        size: int
    }
with
    member private this.ToSeq () =
        let rec toSeq vec =
            seq {
                for i in 0..min vec.nextEmptySlot (vec.elements.Length - 1) do
                    let element = vec.elements[i]
                    
                    match element with
                    | VecNone -> ()
                    | VecItem value -> yield value
                    | VecDeep child ->
                        yield! toSeq child
            }

        toSeq this
    
    interface IEnumerable<'t> with
        member this.GetEnumerator (): IEnumerator<'t> =
            this.ToSeq().GetEnumerator()

        member this.GetEnumerator(): System.Collections.IEnumerator = 
            (this :> IEnumerable<'t>).GetEnumerator() :> System.Collections.IEnumerator

    interface IReadOnlyCollection<'t> with
        member this.Count = this.Length
        
    interface IReadOnlyList<'t> with
        member this.Item with get index = this[index]

    static member internal BufferSize = 32
    
    static member Empty =
        {
            elements = (Array.init vec<'t>.BufferSize (constant VecNone) : VecElement<'t>[])
            elementSize = 1
            nextEmptySlot = 0
            size = 0
        }
    
    /// The number of items contained by the vector.
    member this.Length = this.size
    
    /// Whether this vector contains no items.
    member this.IsEmpty = this.size = 0
    
    member this.Item with get index =
        if this.nextEmptySlot = 0 then collectionWasEmpty()
            
        let struct(elementIndex, passedIndex) = Math.DivRem(index, this.elementSize)
        
        match this.elements[elementIndex] with
        | VecNone -> indexOutOfCollectionBounds()
        | VecItem value -> value
        | VecDeep nestedVector ->
            nestedVector[passedIndex]
    
    member private this.AsString =
        let items =
            this.ToSeq()
            |> Seq.map (sprintf "%A")
            |> String.concat "; "
        
        $"vec {{ %s{items} }}"
        
    override this.ToString () = this.AsString
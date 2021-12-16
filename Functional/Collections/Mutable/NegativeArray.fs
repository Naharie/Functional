namespace Functional

open System
open System.Collections
open System.Collections.Generic

/// An infinitely expandable array like structure that can have negative indexes.
type NegativeArray<'t> (left: 't[], right: 't[], defaultItem: 't option) =
    let leftSide = ResizeArray()
    let rightSide = ResizeArray()

    do
        leftSide.AddRange (Array.rev left)
        rightSide.AddRange right

    /// Creates a new empty negative array.
    new () = NegativeArray ([||], None)
    /// Creates a new empty negative array containing the specified items.
    new (items: 't[]) = NegativeArray([||], items, None)
    /// Creates a new empty negative array containing the specified items and with the specified default item.
    new (items: 't[], defaultItem) = NegativeArray([||], items, defaultItem)
    /// Creates a new empty negative array with the specified default item.
    new (defaultItem: 't option) = NegativeArray ([||], defaultItem)
    /// Creates a new negative array containing the items from the specified sequence.
    new (items: seq<'t>) = NegativeArray (Seq.toArray items, None)
    /// Creates a new negative array with the specified default item, and containing the items from the specified sequence.
    new (items: seq<'t>, defaultItem: 't option) = NegativeArray(Seq.toArray items, defaultItem)

    interface IEnumerable<'t> with
        member this.GetEnumerator () =
            let mutable index = -leftSide.Count - 1

            {
                new IEnumerator<'t> with
                    member _.Current = this.[index]

                interface IEnumerator with
                    member _.Current = box this.[index]
                    member _.MoveNext() =
                        index <- index + 1
                        index < rightSide.Count
                    member _.Reset() =
                        index <- -leftSide.Count

                interface IDisposable with
                    member _.Dispose() = ()
            }
    interface IEnumerable with
        member this.GetEnumerator () =
            (this :> IEnumerable<'t>).GetEnumerator() :> IEnumerator

    /// <summary>Gets or sets the item at the specified index.</summary>
    /// <exception cref="ArgumentException">The negative array was empty.</exception>
    /// <exception cref="IndexOutOfRangeException">The index was out of range and no default item was specified.</exception>
    member _.Item
        with get(index: int) =
            if index < -leftSide.Count || index > rightSide.Count - 1 then
                match defaultItem with
                | Some defaultItem -> defaultItem
                | None ->
                    if leftSide.Count = 0 && rightSide.Count = 0 then
                        raise (ArgumentException "The NegativeArray was empty")
                    else
                        raise (
                            IndexOutOfRangeException $"The index %i{index} is out of bounds (and no default item has been provided); index must be between %i{-leftSide.Count} and %i{rightSide.Count - 1}."
                        )
            else
                if index < 0 then
                    leftSide.[(abs index) - 1]
                else
                    rightSide.[index]
        and set(index: int) value =
            if index < -leftSide.Count || index > rightSide.Count - 1 then
                if leftSide.Count = 0 && rightSide.Count = 0 then
                    raise (ArgumentException "The NegativeArray was empty")
                else
                    raise (
                        IndexOutOfRangeException $"The index %i{index} is out of bounds; index must be between %i{-leftSide.Count} and %i{rightSide.Count - 1}."
                    )
            else
                if index < 0 then
                    leftSide.[(abs index) - 1] <- value
                else
                    rightSide.[index] <- value

    /// <summary>Returns the specified slice of the negative array.</summary>
    /// <param name="lowerBound">The index to start the slice at.</param>
    /// <param name="upperBound">The index to end the slice at.</param>
    member this.GetSlice (lowerBound: int option, upperBound: int option) =
        if leftSide.Count = 0 && rightSide.Count = 0 then
            NegativeArray ([||], defaultItem)
        else
            let lowerBound = defaultArg lowerBound (-leftSide.Count)
            let upperBound = defaultArg upperBound (rightSide.Count - 1)

            let left = [| for index in lowerBound..0 -> this.[index] |]
            let right = [| for index in 0..upperBound -> this.[index] |]

            NegativeArray (left, right, defaultItem)

    /// The left side of the negative array; the minimum index.
    member _.Left = -leftSide.Count
    /// The right side of the negative array; the maximum index.
    member _.Right = rightSide.Count - 1
    /// The number of items the negative array contains.
    member _.Count = leftSide.Count + rightSide.Count

    /// <summary>Adds the specified item to the end of the negative array.</summary>
    /// <param name="item">The item to add.</param>
    member _.Append item =
        rightSide.Add item
    /// <summary>Adds the specified item to the start of the negative array.</summary>
    /// <param name="item">The item to add.</param>
    member _.Prepend item =
        leftSide.Add item

    /// True if the negative array is empty; false otherwise.
    member _.IsEmpty = leftSide.Count = 0 && rightSide.Count = 0

    /// Creates an exact copy of the negative array.
    member _.Copy() =
        ()

[<RequireQualifiedAccess>]
module NegativeArray =
    let init (count: int) (initializer: int -> 't) =
        let result = NegativeArray()

        for index in 1..count do
            result.Append (initializer index)

        result

    let create (count: int) (value: 't) =
        let result = NegativeArray()
        for _ in 1..count do result.Append value
        result
    let copy (negativeArray: NegativeArray<'t>) =
        negativeArray.Copy()

    let toArray (negativeArray: NegativeArray<'t>) =
        let result = Array.zeroCreate negativeArray.Count
        let offset = negativeArray.Left

        for index = negativeArray.Left to negativeArray.Right do
            result.[index + offset] <- negativeArray.[index]

        result
    let ofArray (array: 't[]) = NegativeArray array

    let toSeq (negativeArray: NegativeArray<'t>) =
        negativeArray
        |> toArray
        |> Array.toSeq
    let ofSeq (seq: #seq<'t>) = NegativeArray seq

    let toList (negativeArray: NegativeArray<'t>) =
        negativeArray
        |> toArray
        |> Array.toList
    let ofList (list: 't list) = NegativeArray (List.toArray list)
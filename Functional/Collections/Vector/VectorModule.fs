[<RequireQualifiedAccess>]
module Functional.Collections.Vec

open Functional

#nowarn "0044"

// Specific

let private elmSize = function
    | VecNone -> 0
    | VecItem _ -> 1
    | VecDeep v -> v.size
let private isVecFull vec =
    vec.nextEmptySlot >= vec.elements.Length
    && elmSize vec.elements[^1] = elmSize vec.elements[^0]

/// <summary>
/// Appends a single value to the end of the vector.
/// </summary>
/// <param name="value">The value to append.</param>
/// <param name="vector">The vector to append to.</param>
/// <returns>A copy of the input vector with the specified item appended.</returns>
let rec insert (value: 't) (vector: 't vec) =
    if isVecFull vector then
        let buffer = Array.init vec<'t>.BufferSize (constant VecNone)
        
        buffer[0] <- VecDeep vector
        buffer[1] <- VecItem value

        {
            elements = buffer
            elementSize = vector.size
            nextEmptySlot = 1
            size = vector.size
        }
    else
        match vector.elements[vector.nextEmptySlot] with
        | VecNone ->    
            let buffer = Array.copy vector.elements
            buffer[vector.nextEmptySlot] <- VecItem value

            {
                vector with
                    elements = buffer
                    size = vector.size + 1
                    nextEmptySlot =
                        if vector.elementSize = 1 then
                            vector.nextEmptySlot + 1
                        else
                            vector.nextEmptySlot
            }

        | VecItem _ as wrappedItem ->
            if vector.elementSize = 1 then
                insert value { vector with nextEmptySlot = vector.nextEmptySlot + 1 }
            else
                let buffer = Array.copy vector.elements
                let childBuffer = Array.init 32 (constant VecNone)
                
                childBuffer[0] <- wrappedItem
                childBuffer[1] <- VecItem value
                
                let child = {
                    elements = childBuffer
                    elementSize = 1
                    size = 2
                    nextEmptySlot = 2
                }
                
                buffer[vector.nextEmptySlot] <- VecDeep child   
                { vector with elements = buffer; size = vector.size + 1 }
            
        | VecDeep childVec ->
            if childVec.size >= vector.elementSize then
                insert value { vector with nextEmptySlot = vector.nextEmptySlot + 1 }
            else
                let buffer = Array.copy vector.elements
                let child = insert value childVec
                
                buffer[vector.nextEmptySlot] <- VecDeep child
                
                { vector with elements = buffer; size = vector.size + 1; elementSize = max vector.elementSize child.size }

// Core

/// <summary>
/// An empty vector.
/// </summary>
let inline empty<'t> = vec.Empty

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
let item index (vec: _ vec) = vec.Item index

/// <summary>
/// Produces a sequence that enumerates all the values in the vector.
/// </summary>
/// <param name="vec">The vector to wrap as a sequence</param>
/// <returns>The vector wrapped as a sequence.</returns>
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

// Generic
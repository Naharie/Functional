namespace Functional.Collections

open Functional
open System.Collections.Generic
open Functional.Errors.CollectionErrors

/// An immutable vector with amortized O(1) append and O(log n) lookups and edits.
[<StructuredFormatDisplay "{AsString}">]
type vec<'t> =
    private
    | TODO
with
    member private this.ToSeq () =
        let rec toSeq vec =
            seq {
                failwith "TODO"
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

    /// The number of items contained by the vector.
    member this.Length =
        failwith "TODO"
    
    /// Whether this vector contains no items.
    member this.IsEmpty = this.Length = 0
    
    member this.Item with get index =
        failwith "TODO"
    
    member private this.AsString =
        let items =
            this.ToSeq()
            |> Seq.map (sprintf "%A")
            |> String.concat "; "
        
        $"vec {{ %s{items} }}"
        
    override this.ToString () = this.AsString
namespace Functional

open System
open System.Collections.Generic
open Functional.Errors.CollectionErrors
open Microsoft.FSharp.Core

[<AutoOpen>]
module private FingerTreeInternals =
    [<StructuredFormatDisplay "{AsString}">]
    type Node<'t> =
        | Node2 of 't * 't
        | Node3 of 't * 't * 't
    with
        member private this.AsString =
            match this with
            | Node2 (a, b) -> $"{a}, {b}"
            | Node3 (a, b, c) -> $"{a}, {b}, {c}"

        override this.ToString () = this.AsString

    [<StructuredFormatDisplay "{AsString}">]
    type Digit<'t> =
        | One of 't
        | Two of 't * 't
        | Three of 't * 't * 't
        | Four of 't * 't * 't * 't
    with
        member private this.AsString =
            match this with
            | One a -> if isNull (box a) then "" else a.ToString()
            | Two (a, b) -> $"{a}, {b}"
            | Three (a, b, c) -> $"{a}, {b}, {c}"
            | Four (a, b, c, d) -> $"{a}, {b}, {c}, {d}"
        member this.Item with get index =
            match this with
            | One a when index = 0 -> a
            
            | Two (a, _) when index = 0 -> a
            | Two (_, b) when index = 1 -> b
            
            | Three(a, _, _) when index = 0 -> a
            | Three(_, b, _) when index = 1 -> b
            | Three(_, _, c) when index = 2 -> c
            
            | Four(a, _, _, _) when index = 0 -> a
            | Four(_, b, _, _) when index = 1 -> b
            | Four(_, _, c, _) when index = 2 -> c
            | Four(_, _, _, d) when index = 3 -> d
            
            | _ -> indexOutOfRangeMustBeWithinCollection()
        
        override this.ToString () = this.AsString

    module Node =
        let inline toDigit node =
            match node with
            | Node2 (a, b) -> Two(a, b)
            | Node3 (a, b, c) -> Three(a, b, c)

        let inline map mapping node =
            match node with
            | Node2 (a, b) -> Node2 (mapping a, mapping b)
            | Node3 (a, b, c) -> Node3 (mapping a, mapping b, mapping c)

    module Digit =
        let append value digit =
            match digit with
            | One a -> Two(a, value)
            | Two (a, b) -> Three(a, b, value)
            | Three (a, b, c) -> Four(a, b, c, value)

            // Just ignore it; this case should never be called.
            | Four _ -> digit
        let prepend value digit =
            match digit with
            | One b -> Two(value, b)
            | Two (b, c) -> Three(value, b, c)
            | Three (b, c, d) -> Four(value, b, c, d)

            // Just ignore it; this case should never be called.
            | Four _ -> digit

        let inline map mapping digit =
            match digit with
            | One a -> One (mapping a)
            | Two (a, b) -> Two (mapping a, mapping b)
            | Three (a, b, c) -> Three (mapping a, mapping b, mapping c)
            | Four (a, b, c, d) -> Four (mapping a, mapping b, mapping c, mapping d)
        let inline rev digit =
            match digit with
            | One _ -> digit
            | Two (a, b) -> Two (b, a)
            | Three (a, b, c) -> Three (c, b, a)
            | Four (a, b, c, d) -> Four (d, c, b, a)

/// An efficient functional data structure designed to be used in implementing other data structures.
/// Provides amortized O(1) access to the leaves of the tree.
[<StructuredFormatDisplay "{AsString}">]
[<CustomEquality; NoComparison>]
type FingerTree<'t> =
    private
    | Blank
    | Single of 't
    | Deep of leftValue:Digit<'t> * nestedTree:FingerTree<Node<'t>> * rightValue:Digit<'t>
with
    static member private PromoteDigit digit =
        match digit with
        | One a -> Single a
        | Two (a, b) -> Deep(One a, Blank, One b)
        | Three (a, b, c) -> Deep (One a, Blank, Two (b, c))
        | Four (a, b, c, d) -> Deep (Two (a, b), Blank, Two (c, d))
    
    member private this.ViewV(): ('t * FingerTree<'t>) voption =
        match this with
        | Blank -> ValueNone
        | Single value -> ValueSome(value, Blank)
        
        | Deep(One value, middle, right) ->
            let rest =
                match middle.ViewV() with
                | ValueNone -> FingerTree<'t>.PromoteDigit right
                | ValueSome (node, rest) ->
                    Deep (Node.toDigit node, rest, right)
            
            ValueSome(value, rest)
        
        | Deep(Two (a, b), middle, right) ->
            ValueSome(a, Deep (One b, middle, right))
        
        | Deep(Three (a, b, c), middle, right) ->
            ValueSome(a, Deep (Two (b, c), middle, right))
        
        | Deep(Four (a, b, c, d), middle, right) ->
            ValueSome(a, Deep (Three (b, c, d), middle, right))
    
    interface IEnumerable<'t> with
        member this.GetEnumerator () =
            (Seq.unfold (fun (rest: FingerTree<_>) ->
                match rest.ViewV() with
                | ValueSome (value, rest) -> Some (value, rest)
                | ValueNone -> None
            ) this).GetEnumerator()

        member this.GetEnumerator(): Collections.IEnumerator = 
            (this :> IEnumerable<'t>).GetEnumerator() :> Collections.IEnumerator
    interface IReadOnlyCollection<'t> with
        member this.Count = this.Length
    
    member private this.AsString =
        match this with
        | Blank -> "()"
        | Single value -> $"({value})"
        | Deep (prefix, tree, suffix) -> $"({prefix}, {tree}, {suffix})"

    /// Determines whether this tree contains any elements.
    member this.IsEmpty =
        match this with
        | Blank -> true
        | Single _ -> false
        | Deep _ -> false

    /// The total number of leaves in the tree.
    member this.Length =
        // Duplicated from FingerTreeModule
        // Update the code there first, then copy paste here and fix.
        let rec count total (tree: FingerTree<'t>) =
            match tree with
            | Blank -> 0
            | Single _ -> 1
            
            | Deep(One _, middle, right) ->
                let rest =
                    match middle.ViewV() with
                    | ValueNone -> FingerTree<_>.PromoteDigit right
                    | ValueSome (node, rest) ->
                        Deep (Node.toDigit node, rest, right)
                
                count (total + 1) rest
            
            | Deep(Two _, middle, right) ->
                let rest =
                    match middle.ViewV() with
                    | ValueNone -> FingerTree<_>.PromoteDigit right
                    | ValueSome (node, rest) ->
                        Deep (Node.toDigit node, rest, right)
                
                count (total + 2) rest
            
            | Deep(Three _, middle, right) ->
                let rest =
                    match middle.ViewV() with
                    | ValueNone -> FingerTree<_>.PromoteDigit right
                    | ValueSome (node, rest) ->
                        Deep (Node.toDigit node, rest, right)
                
                count (total + 3) rest
            
            | Deep(Four _, middle, right) ->
                let rest =
                    match middle.ViewV() with
                    | ValueNone -> FingerTree<_>.PromoteDigit right
                    | ValueSome (node, rest) ->
                        Deep (Node.toDigit node, rest, right)
                
                count (total + 4) rest
        
        count 0 this

    override this.ToString () = this.AsString

    override this.Equals (other: obj): bool =
        match other with
        | :? FingerTree<'t> as other ->
            let rec compare (a: _ FingerTree) (b: _ FingerTree) =
                match a.ViewV() with
                | ValueNone ->
                    match b.ViewV() with
                    | ValueNone -> true
                    | ValueSome _ -> false

                | ValueSome (va, a) ->
                    match b.ViewV() with
                    | ValueNone -> false
                    | ValueSome (vb, b) ->
                        if Unchecked.equals va vb then compare a b else false

            compare this other

        | _ -> false

    override this.GetHashCode () = Unchecked.hash [|
       let mutable tree = this
       
       while tree <> Blank do
           match tree.ViewV() with
           | ValueNone -> tree <- Blank
           | ValueSome(item, rest) ->
               yield item
               tree <- rest
   |]
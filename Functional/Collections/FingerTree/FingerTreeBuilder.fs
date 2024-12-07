module Functional.FingerTree.Builder

open Functional
open Functional.ComputationBuilders.CollectionBuilders

// So we can use FingerTreeShared without warnings
#nowarn "0044"

type FingerTreeBuilder() =
    inherit CollectionBuilder()
    
    member inline _.Yield v =
        fun (s: FingerTree<'t>) -> FingerTreeShared.insertRight v s

    member inline this.YieldFrom vs =
        this.For(vs, this.Yield)
        
    member inline _.Run (f: FingerTree<'t> -> FingerTree<'t>) =
        f Blank
namespace Functional.Collections

open Functional.ComputationBuilders.CollectionBuilders

type ResizeArrayBuilder () =
    inherit CollectionBuilder()
    
    member inline _.Yield v =
        fun (s: ResizeArray<'t>) -> s.Add(v); s
        
    member inline this.YieldFrom vs =
        this.For(vs, this.Yield)
        
    member inline _.Run (f: ResizeArray<'t> -> ResizeArray<'t>) =
        f (ResizeArray())
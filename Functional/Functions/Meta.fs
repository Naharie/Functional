[<AutoOpen>]
module Functional.MetaFunctionBuilder

type MetaValue = | MetaValue
with
    static member inline (@+) (MetaValue, (a: 't -> 'u, b: 'v)) = fun x -> (a x) + b
    static member inline (+@) (MetaValue, (a: 'v, b: 't -> 'u)) = fun x -> a + (b x)
    
    static member inline (@*) (MetaValue, (a: 't -> 'u, b: 'v)) = fun x -> (a x) * b
    static member inline ( *@) (MetaValue, (a: 'v, b: 't -> 'u)) = fun x -> a * (b x)
    
let __ = id

let inline (@+) a b = MetaValue @+ (a, b)
let inline (+@) a b = MetaValue +@ (a, b)

let inline (@*) a b = MetaValue @* (a, b)
let inline ( *@) a b = MetaValue *@ (a, b)
[<Microsoft.FSharp.Core.AutoOpen>]
module Functional.UtilityFunctions

let inline apply f x = f x
let inline apply2 f (x, y) = f x y

let inline applyBack f y x  = f x y
let inline applyBack3 f c b a = f a b c

let inline applyABC f a b c = f a b c
let inline applyACB f a c b = f a b c
let inline applyBAC f b a c = f a b c
let inline applyBCA f b c a = f a b c
let inline applyCAB f c a b = f a b c
let inline applyCBA f c b a = f a b c

let inline (@@) f b a = f a b
let inline (@@@) f c b a = f a b c

let inline constant x _ = x

let note note x =
    printfn $"%s{note}"
    x

let show label x =
    printfn $"%s{label}: %A{x}"
    x
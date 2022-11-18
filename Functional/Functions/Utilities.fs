[<Microsoft.FSharp.Core.AutoOpen>]
module Functional.UtilityFunctions

let apply f x = f x
let apply2 f (x, y) = f x y

let constant x _ = x
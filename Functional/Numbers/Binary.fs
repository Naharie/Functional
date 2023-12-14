[<Microsoft.FSharp.Core.RequireQualifiedAccess>]
module Functional.Binary

open System

let fromInt (value: int) =
    Convert.ToString(value, 2)  
let toInt (value: string) =
    Convert.ToInt32(value, 2)
    
let negate (value: string) =
    value
    |> String.map (function
        | '0' -> '1'
        | '1' -> '0'
        | other -> other
    )
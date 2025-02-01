module TestBed.Main

open Functional.Collections

let code() =
    let mutable v = Vec.empty
    
    for i in 1..100 do
        v <- Vec.insert i v
       
    printfn $"%A{v}"
    
    ()
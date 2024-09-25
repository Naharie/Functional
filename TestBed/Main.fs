module TestBed.Main

open Functional

let code () =
    imperative {
        let list = [ 1; 3; 5; 8; 9; 10 ]
        
        for i in list do
            if i % 2 = 0 then
                printfn "Found an even number: %i" i
                do! _break
            else
                printfn "Testing %i" i
        
        printfn "This still happens!"
        return ()
    }
module TestBed.Main

open Functional

let code () =
    let list = [ 2; 3; 5; 4; 7 ]
    
    imperative {
        for i in list do
            if i % 2 = 0 then
                printfn $"Found an even number: %i{i}"
                do! _continue
            elif i % 5 = 0 then
                do! _break

            printfn $"Testing %i{i}"
        
        printfn "This still happens!"
        return ()
    }
   
    ()
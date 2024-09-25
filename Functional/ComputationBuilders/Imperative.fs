module Functional.ComputationBuilders.Imperative

open System

type Imperative<'t> = unit -> 't option

type ImperativeBuilder() =
    member _.Return value = fun () -> Some value
    member _.Zero () = fun () -> None
    
    member _.Delay (f: unit -> _ Imperative) =  (fun () -> f()())
  
    member _.Combine (a: _ Imperative, b: _ Imperative) = (fun () ->
        match a() with 
        | Some(v) -> Some(v) 
        | _ -> b()
    )
    
    member this.TryWith (body: _ Imperative, handler : exn -> _ Imperative) = (fun () ->
        try
            body()
        with
        | error ->
            (handler error)()
    )
    member this.TryFinally (body: _ Imperative, handler) = (fun () ->
        try
            body()
        finally
            handler()
    )

    member this.Using (resource: #IDisposable, body: 'e -> _ Imperative) =
        this.TryFinally(
            (body resource),
            (fun () ->
                match resource with
                | null -> ()
                | disposable -> disposable.Dispose()
            )
        )
        
    member this.While (condition, body: _ Imperative) =
        if not <| condition() then this.Zero()
        else
            this.Combine(body, this.Delay(fun () -> this.While(condition, body)))
    member this.For (sequence: seq<_>, body) =
        this.Using (
            sequence.GetEnumerator (),
            (fun enumerator ->
                this.While (
                    enumerator.MoveNext,
                    this.Delay (fun () -> body enumerator.Current)
                )
            )
        )

    member _.Run (work: _ Imperative) =
        match work() with
        | Some v -> v
        | None ->
            failwith "Imperative computation expression did not return a value!"
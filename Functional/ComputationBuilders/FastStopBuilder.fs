// This module is not called "Imperative" to stop it from auto completing above the imperative {} expression.
module Functional.ComputationBuilders.FastStopBuilder

open System

type ImperativeResult<'t> =
    | Returned of 't
    | DidNotReturn
    | SkippedIteration
    | StoppedIteration
type Imperative<'t> = unit -> ImperativeResult<'t>

type ImperativeBuilder() =
    member _.Return value = fun () -> Returned value
    member _.Zero () = fun () -> DidNotReturn
    
    member _.Delay (f: unit -> _ Imperative) = fun () -> f()()
  
    member _.Bind (value: ImperativeResult<_>, f: unit -> Imperative<_>) = fun () ->
        match value with
        | Returned t -> Returned t
        | DidNotReturn -> f()()
        | SkippedIteration -> SkippedIteration
        | StoppedIteration -> StoppedIteration
    
    member _.Combine (a: _ Imperative, b: _ Imperative) = (fun () ->
        match a() with 
        | Returned v  -> Returned v
        | SkippedIteration -> SkippedIteration
        | StoppedIteration -> StoppedIteration
        | DidNotReturn -> b()
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

    member this.Using (resource: #IDisposable, body: 'e -> _ Imperative) = (fun () ->
        use _ = resource
        body resource
    )
        
    member this.While (condition, body: _ Imperative) =
        if condition() then
            (fun () ->
                match body() with 
                | Returned v  -> Returned v
                | StoppedIteration -> DidNotReturn
                | DidNotReturn | SkippedIteration -> this.While(condition, body)()
            )
        else
            this.Zero()
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
        | Returned v -> v
        | DidNotReturn | SkippedIteration | StoppedIteration ->
            failwith "Imperative computation expression did not return a value!"
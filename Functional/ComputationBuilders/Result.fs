namespace Functional.ComputationBuilders

open System
open Functional.ComputationBuilders

type ResultBuilder () =
    member _.Return value = Value (Ok value)
    member _.ReturnFrom value = Value value

    member this.Bind (value, func) =
        match value with
        | Value value -> func value
        | Future action ->
            Future (fun () -> this.Bind (action (), func))
        | Error error -> Error error
    member _.Bind (value, func) =
        Future (fun () ->
            match value with
            | Ok value -> func value
            | Result.Error _ -> Value value
        )

    member _.Delay func = Future func
    member _.Zero () = Value (Ok ())

    member this.Combine (f1: _ Computation, f2) =
        this.Bind (f1, fun _ -> f2)

    member this.TryWith (body: _ Computation, handler) =
        Future (fun () ->
            try
                match body with
                | Value _ | Error _ -> body
                | Future f -> f()
            with
            | error ->
                handler error
        )
    member this.TryFinally (body, handler) =
        Future (fun () ->
            try
                match body with
                | Value _ | Error _ -> body
                | Future f -> f()
            finally
                handler()
        )

    member this.Using (resource: #IDisposable, body) =
        Future (fun () ->
            use _ = resource
            body resource
        )

    member this.While (condition, body: _ Computation) =
        if condition() then
            this.Bind(body, fun () -> this.While (condition, body))
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
        
    member this.Run (computation: Result<'t, 'e> Computation) =
        let rec go expr =
            match expr with
            | Value x -> x
            | Future f -> go(f())
            | Error e -> raise e
        
        go computation
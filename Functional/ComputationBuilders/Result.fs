// Based on the maybe builder by Steve Horsfield
// Blog post: https://stevehorsfield.wordpress.com/2009/09/06/f-delayed-compositional-maybe-monad-workflow-full-source/

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
            Future (fun () ->
                this.Bind (action (), func)
            )
        | Error error -> Error error
    member _.Bind (value, func) =
        Future (fun () ->
            match value with
            | Ok value -> func value
            | Result.Error _ -> Value value
        )

    member _.Delay func = Future func
    member _.Zero () = Value (Ok ())

    member this.Combine (f1: Computation<_>, f2) =
        this.Bind (f1, fun _ -> f2)

    member this.Catch (body: 't Computation) =
        match body with
        | Value _ -> Value body
        | Future action ->
            Future (fun () ->
                let bodyResult =
                    try
                        action()
                    with
                    | error -> Error error
                    |> Value

                this.Catch (this.Bind (bodyResult, id))
            )
        | Error error -> Error error

    member this.TryWith (body, handler) =
        this.Bind (
            (this.Catch body),
            (function
                | Error error -> handler error
                | other -> other
            )
        )
    member this.TryFinally (body, finallyHandler) =
        this.Bind (
            (this.Catch body),
            (fun result ->
                finallyHandler()
                match result with
                | Error error -> raise error
                | other -> other
            )
        )

    member this.Using (resource: #IDisposable, body) =
        this.TryFinally (
            (body resource),
            (fun () ->
                match resource with
                | null -> ()
                | disposable -> disposable.Dispose()
            )
        )

    member this.While (condition, body: _ Computation) =
        if condition() then
            this.Bind (body, (fun _ -> this.While (condition, body)))
        else
            this.Zero ()
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

    member _.Run work =
        let rec eval work =
            match work with
            | Value option -> option
            | Future action -> eval <| action()
            | Error error -> raise error

        eval work
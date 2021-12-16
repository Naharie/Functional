// Based on the maybe builder by Steve Horsfield
// Blog post: https://stevehorsfield.wordpress.com/2009/09/06/f-delayed-compositional-maybe-monad-workflow-full-source/

namespace Functional.ComputationBuilders

open System
open Functional.ComputationBuilders

type MaybeBuilder() =
    member _.Return value = Value (Some value)
    
    member _.ReturnFrom (value: 't option) = Value value
    member _.ReturnFrom (value: Nullable<'t>) =
        Value (if value.HasValue then Some value.Value else None)
    member _.ReturnFrom (value: (bool * 't)) =
        Value (if fst value then Some (snd valumodule Functional.Computations.Result

e) else None)

    member this.Bind (value: 't Computation, func) =
        match value with
        | Value value -> Future (fun () -> func value)
        | Future action -> Future (fun () -> this.Bind (action (), func))
        | Error error -> Error error
    member _.Bind (value: 'a option, func: 'a -> 'b option Computation) =
        Future (fun () ->
            match value with
            | Some value -> func value
            | None -> Value None
        )
    member _.Bind (value: Nullable<'t>, func) =
        Future (fun () ->
            if value.HasValue then func value.Value
            else Value None
        )
    member _.Bind (value : (bool * 't), func) =
        Future (fun () -> if fst value then func (snd value) else Value None)
    member _.Bind (value : bool, func) =
        Future (fun () -> if value then func () else Value None)

    member _.Delay f = Future f
    member _.Zero () = Value (Some ())

    member this.Combine (f1: _ Computation, f2) =
        this.Bind (f1, fun _ -> f2)

    member this.Catch (body: 'T Computation) =
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
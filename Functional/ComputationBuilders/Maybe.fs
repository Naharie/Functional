namespace Functional.ComputationBuilders

open System

type MaybeBuilder() =
    member _.Return value = Value (Some value)
    
    member _.ReturnFrom (value: 't option) = Value value
    member _.ReturnFrom (value: Nullable<'t>) =
        Value(if value.HasValue then Some value.Value else None)
    member _.ReturnFrom (value: bool * 't) =
        Value(if fst value then Some (snd value) else None)

    member this.Bind (value: _ option Computation, func: 'a -> 'b option Computation) =
        Future (fun () ->
            match value with
            | Value simple ->
                match simple with
                | Some x -> func x
                | None -> Value None
            | Future delayed ->
                this.Bind(delayed(), func)
            | Error e -> Error e
        ) 
    member this.Bind (value: 't option, func) =
        Future (fun () ->
            match value with
            | Some x -> func x
            | None -> Value None
        )
    member this.Bind (value: 't voption, func) =
        Future (fun () ->
            match value with
            | ValueSome x -> func x
            | ValueNone -> Value None
        )
    member this.Bind (value: Nullable<'t>, func) =
        this.Bind(Value (if value.HasValue then Some value.Value else None), func)
    member this.Bind (value : bool * 't, func) =
        this.Bind(Value (if fst value then Some (snd value) else None), func)
    member this.Bind (value : bool, func) =
        this.Bind(Value (if value then Some () else None), func)

    member _.Delay func = Future func
    member _.Zero () = Value (Some ())

    member this.Combine (f1: _ Computation, f2: _ Computation) =
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

    member this.Run (computation: 't option Computation) =
        let rec go expr =
            match expr with
            | Value x -> x
            | Future f -> go(f())
            | Error e -> raise e
        
        go computation
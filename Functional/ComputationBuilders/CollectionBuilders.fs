module Functional.ComputationBuilders.CollectionBuilders

open System

type CollectionBuilder () =
    member inline _.Combine ([<InlineIfLambda>] f1: 'state -> 'state, [<InlineIfLambda>] f2: 'state -> 'state) =
        f1 >> f2

    member inline _.Delay ([<InlineIfLambda>] f: unit -> 'state -> 'state) =
        f()

    member inline _.Zero () = fun state -> state

    member inline _.While ([<InlineIfLambda>] condition: unit -> bool, [<InlineIfLambda>] body: 'state -> 'state) =
        fun startingState ->
            let mutable state = startingState
            
            while condition() do
                state <- body state
            
            state

    member inline _.TryWith ([<InlineIfLambda>] body: 'state -> 'state, [<InlineIfLambda>] handler: exn -> 'state -> 'state) =
        fun state ->
            try
                body state
            with
            | error ->
                handler error state

    member inline _.TryFinally ([<InlineIfLambda>] body: 'state -> 'state, compensation: unit -> unit) =
        fun state ->
            try body state finally compensation()

    member inline this.Using (disposable: #IDisposable, [<InlineIfLambda>] body: #IDisposable -> 'state -> 'state) =
        this.TryFinally(body disposable, fun () ->
            if not (isNull (box disposable)) then
                disposable.Dispose()
        )
        
    member inline this.For (sequence: seq<_>, [<InlineIfLambda>] body: 'v -> 'state -> 'state) =
        this.Using(sequence.GetEnumerator(), fun enumerator ->
            this.While(enumerator.MoveNext, fun state ->
                body enumerator.Current state
            )
        )
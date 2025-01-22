[<AutoOpen>]
module Functional.AsyncPrelude

open System
open System.Collections.Generic
open System.Threading.Tasks

type Microsoft.FSharp.Control.AsyncBuilder with
    member _.Bind(task: Task<'t>, handler: 't -> Async<'r>) =
        async.Bind(Async.AwaitTask task, handler)

    member this.Using(resource: 't :> IAsyncDisposable, body: 't -> Async<'r>) =
        async {
            try
                let! result = body resource
                do! Async.AwaitTask(resource.DisposeAsync().AsTask())
                return result
            with _ ->
                do! Async.AwaitTask(resource.DisposeAsync().AsTask())
        }

    member this.For(sequence: IAsyncEnumerable<'t>, handler: 't -> Async<unit>) =
        async {
            use enumerator = sequence.GetAsyncEnumerator()

            while! enumerator.MoveNextAsync().AsTask() do
                do! handler enumerator.Current
        }

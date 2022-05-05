[<RequireQualifiedAccess>]
module Functional.Async

open System

/// Sequentially executes the sequence of asynchronous actions.
let Sequential (computations: #seq<Async<'t>>): Async<'t[]> =
    async {
        let result = ResizeArray()
        
        for computation in computations do
            let! value = computation
            result.Add value
            
        return result.ToArray()
    }

/// Waits once for the next event from the observable.
let AwaitObservable (observable: IObservable<'t>) =
    Async.FromContinuations (fun (finish, _, _) ->
        Observable.once finish observable
        |> ignore
    )

/// Asynchronously applies the given function to the result of the asynchronous value.
let map mapping ``async value`` =
    async {
        let! result = ``async value``
        return mapping result
    }

/// Applies the given function ot the result of the asynchronous value.
let bind binder computation =
    async.Bind (computation, binder)

/// Creates an async computation that fails with the specified message.
let fail message : 't Async =
    async {
        failwith message
        return Unchecked.defaultof<'t>
    }

/// Returns the result of the first computation to complete.
/// If none of the computations return, then None is returned instead.
let tryFirst (computations: #seq<Async<'t>>) =
    computations
    |> Seq.map (map Some)
    |> Async.Choice
    |> bind (fun wrapped ->
        async  {
            match wrapped with
            | Some result ->
                return Some result
            | None ->
                return None
        }
    )

/// Returns the result of the first computation to complete.
/// If none of the computations return, then an error is thrown.
let first (computations: #seq<Async<'t>>) =
    computations
    |> Seq.map (map Some)
    |> Async.Choice
    |> bind (fun wrapped ->
        async  {
            match wrapped with
            | Some result ->
                return result
            | None ->
                failwith "All computations failed."
                return Unchecked.defaultof<'t>
        }
    )
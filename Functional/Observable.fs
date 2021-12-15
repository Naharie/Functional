[<RequireQualifiedAccess>]
module Functional.Observable

open System

/// Notifies a provider when a new value is received.
/// The input to the observer function is the cancellation function.
let watch (observer: (unit -> unit) -> IObserver<'t>) (observable: IObservable<'t>) =
    let mutable subscription = None
    let cancel () =
        match subscription with
        | Some (inner: IDisposable) ->
            inner.Dispose()
            subscription <- None
        | None -> ()

    subscription <- Some <| observable.Subscribe (observer cancel)
    { new IDisposable with member _.Dispose () = cancel() }

/// Attaches a handler to the observable that waits for the next value and then unsubscribes.
let once (handler: 't -> unit) observable =
    let mutable subscription = None
    
    subscription <- Some (observable
    |> Observable.subscribe (fun value ->
        match subscription with
        | Some (inner: IDisposable) ->
            inner.Dispose()
            subscription <- None

            handler value
        | None -> ()
    ))
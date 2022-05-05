[<RequireQualifiedAccess>]
module Functional.Observable

open System
open Purity

/// Notifies a provider when a new value is received.
/// The input to the observer function is the cancellation function.
let watch (observer: (unit -> unit) -> IObserver<'t>) (observable: IObservable<'t>) =
    withRecursive (fun (subscription: IDisposable Lazy) ->
        let cancel () = subscription.Value.Dispose()
        observable.Subscribe (observer cancel)
    )

/// Attaches a handler to the observable that waits for the next value and then unsubscribes.
let once (handler: 't -> unit) observable =
    withRecursive (fun (subscription) ->
        Observable.subscribe (fun value ->
            if not subscription.IsValueCreated then
                subscription.Value.Dispose()
                handler value
        ) observable
    )
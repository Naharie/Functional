[<RequireQualifiedAccess>]
module Functional.Observable

open System
open Purity

/// <summary>
/// Subscribes an observer to an observable while passing a cancellation function.
/// </summary>
/// <param name="observer">A function that taking a cancellation function provides a <see cref="System.IObserver" /> </param>
/// <param name="observable">The observable that the resulting observer is subscribed to.</param>
let watch (observer: (unit -> unit) -> IObserver<'t>) (observable: IObservable<'t>) =
    ensureNotNull (nameof observable) observable
    
    withRecursive (fun (subscription: IDisposable Lazy) ->
        let cancel () = subscription.Value.Dispose()
        observable.Subscribe (observer cancel)
    )

/// <summary>
/// Attaches a handler to the observable that waits for the next value and then unsubscribes.
/// </summary>
/// <param name="handler">A function that will be called once with the next value from the observable.</param>
/// <param name="observable">The observable that the value will come from.</param>
let once (handler: 't -> unit) observable =
    ensureNotNull (nameof observable) observable
    
    let mutable alive = true
    let cancel (disposable: IDisposable) =
        alive <- false
                
        try
            disposable.Dispose()
        with
        | _ -> ()
    
    let disposable = withRecursive (fun subscription ->
        Observable.subscribe (fun value ->
            if alive then
                handler value
                cancel subscription.Value
        ) observable
    )
    
    {
        new IDisposable with
            member _.Dispose () = cancel disposable
    }
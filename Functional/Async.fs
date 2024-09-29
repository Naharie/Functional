[<RequireQualifiedAccess>]
module Functional.Async

open System

/// <summary>
/// Sequentially executes the sequence of asynchronous actions.
/// </summary>
/// <param name="computations">The sequence of computations to execute.</param>
/// <returns>An array of the results of the computations.</returns>
let Sequential (computations: #seq<Async<'t>>): Async<'t[]> =
    async {
        let result = ResizeArray()
        
        for computation in computations do
            let! value = computation
            result.Add value
            
        return result.ToArray()
    }

/// <summary>
/// Waits for and then returns the next value from the observable.
/// </summary>
/// <param name="observable">The observable to be waited upon.</param>
/// <returns>An asynchronous computation that will return the next value from the observable.</returns>
let AwaitObservable (observable: IObservable<'t>) =
    Async.FromContinuations (fun (finish, _, _) ->
        Observable.once finish observable
        |> ignore
    )

/// <summary>
/// Asynchronously applies the given function to the result of the asynchronous value.
/// </summary>
/// <param name="mapping">The mapping to apply to the wrapped value.</param>
/// <param name="async value">The computation wrapping the value to apply the mapping function to.</param>
/// <returns>A new asynchronous computation containing the result of the application of the mapping function.</returns>
let map mapping ``async value`` =
    async {
        let! result = ``async value``
        return mapping result
    }

/// <summary>
/// Applies the given function to the result of the asynchronous value.
/// </summary>
/// <param name="binder">The function to apply to the unwrapped value.</param>
/// <param name="computation">The wrapped value the function will be applied to.</param>
/// <returns>The result of the <paramref name="binder"/> function.</returns>
let bind binder computation =
    async.Bind (computation, binder)

/// <summary>
/// Creates an async computation that fails with the specified exception.
/// </summary>
/// <param name="error">The exception to raise inside the asynchronous task.</param>
/// <returns>An asynchronous task that fails with the specified exception.</returns>
let fail error : 't Async =
    async {
        raise error
        return Unchecked.defaultof<'t>
    }

/// <summary>
/// Returns the result of the first computation to complete.
/// If none of the computations return, then None is returned instead.
/// </summary>
/// <param name="computations">A set of computations from which the result of the first computation to complete will be returned.</param>
/// <returns>The result of the first computation to complete, or None if no computation completes successfully.</returns>
let tryFirst (computations: #seq<Async<'t>>) =
    computations
    |> Seq.map (map Some)
    |> Async.Choice

/// <summary>
/// Returns the result of the first computation to complete.
/// If none of the computations return, then an exception is thrown.
/// </summary>
/// <param name="computations">A set of computations from which the result of the first computation to complete will be returned.</param>
/// <returns>The result of the first computation to complete.</returns>
/// <exception cref="Functional.Exceptions.NoValidItemException">None of the specified computations successfully completed and returned a value.</exception>
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
                noSuchItem "All of the asynchronous computations in the input sequence failed to return a value."
                return Unchecked.defaultof<'t>
        }
    )
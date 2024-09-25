[<RequireQualifiedAccess>]
module Functional.Lazy
    /// <summary>
    /// Forces the evaluation of the specified lazy value.
    /// </summary>
    /// <param name="lazy">The lazy value to force evaluation of.</param>
    /// <returns>The unwrapped result of the lazy value.</returns>
    let inline force (``lazy``: _ Lazy) = ``lazy``.Value

    /// <summary>
    /// Applies the specified function to the value wrapped in the lazy.
    /// </summary>
    /// <param name="mapping">The function to apply to the wrapped value.</param>
    /// <param name="lazy">The lazy value wrapping the value to apply the function to.</param>
    /// <returns>A new lazy value wrapping the mapped value.</returns>
    let map mapping ``lazy`` =
        lazy (mapping <| force ``lazy``)

    /// <summary>
    /// Returns the result of applying the binding to the lazy value.
    /// The execution of the binding is delayed untill the value is requested.
    /// </summary>
    /// <param name="binding">The binding to apply. This must return a new lazy value.</param>
    /// <param name="lazy">The lazy value to unwrap and apply the binding to.</param>
    /// <returns>The result of the binding function.</returns>
    let bind binding ``lazy`` =
        lazy (
            ``lazy``
            |> force
            |> binding
            |> force
        )
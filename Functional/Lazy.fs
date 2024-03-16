[<RequireQualifiedAccess>]
module Functional.Lazy
    /// Forces the evaluation of the specified lazy value.
    let inline force (``lazy``: _ Lazy) = ``lazy``.Value

    /// Applies the specified function to the value wrapped in the lazy.
    let map mapping ``lazy`` =
        lazy (mapping <| force ``lazy``)

    /// Returns the result of applying the binding to the lazy value.
    /// The application is delayed until the value is requested.
    let bind binding ``lazy`` =
        lazy (
            ``lazy``
            |> force
            |> binding
            |> force
        )
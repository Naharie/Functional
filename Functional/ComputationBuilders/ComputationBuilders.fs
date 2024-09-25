namespace Functional.ComputationBuilders

/// Represents an intermediate computation in a computation expression.
type Computation<'t> =
    | Value of 't
    | Future of (unit -> 't Computation)
    | Error of exn
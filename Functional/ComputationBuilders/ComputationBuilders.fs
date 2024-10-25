namespace Functional.ComputationBuilders

/// Represents an intermediate computation in a computation expression.
[<Struct>]
type Computation<'t> =
    | Value of concrete:'t
    | Future of future:(unit -> 't Computation)
    | Error of error:exn
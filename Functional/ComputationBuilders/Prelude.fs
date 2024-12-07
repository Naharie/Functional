[<AutoOpen>]
module Functional.ComputationsPrelude

open Functional.ComputationBuilders
open Functional.ComputationBuilders.FastStopBuilder

/// Builds an Option<'t> using the workflow syntax.
let maybe = MaybeBuilder ()

/// Builds a Result<'T, 'E> type using the workflow syntax.
let result = ResultBuilder()

/// <summary>
/// A computation expression that runs imperatively, that is, exits as soon as return is called.
/// The expression <em>must</em> return a value, or it <em>will</em> error, but it is okay to return unit.
/// Loops can be exited with <code>do! _break</code> or a single iteration can be skipped with <code>do! _continue</code>
/// </summary>
let imperative = ImperativeBuilder()
let _break = StoppedIteration
let _continue = SkippedIteration
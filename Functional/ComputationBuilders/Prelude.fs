[<AutoOpen>]
module Functional.ComputationsPrelude

open Functional.ComputationBuilders
open Functional.ComputationBuilders.Imperative

/// Builds an Option<'t> using the workflow syntax.
let maybe = MaybeBuilder ()

/// Builds a Result<'T, 'E> type using the workflow syntax.
let result = ResultBuilder()

/// A computation expression that exits as soon as return is called.
let imperative = ImperativeBuilder()
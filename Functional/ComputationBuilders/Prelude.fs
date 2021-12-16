[<AutoOpen>]
module Functional.ComputationsPrelude

open Functional.ComputationBuilders

/// Builds an Option<'t> using the workflow syntax.
let maybe = MaybeBuilder ()

/// Builds a Result<'T, 'E> type using the workflow syntax.
let result = ResultBuilder()
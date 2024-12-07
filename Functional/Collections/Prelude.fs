[<AutoOpen>]
module Functional.FingerTreePrelude

open Functional.Collections
open Functional.FingerTree.Builder

/// <summary>
/// Builds a ResizeArray using a computation expression, much like <c>seq { ... }</c>.
/// </summary>
/// <example>
/// An expression such as <c>resizeArray { 1; 2; 3 }</c> compiles to
/// <code>
/// let r = ResizeArray() in
///     r.Add(1);
///     r.Add(2);
///     r.Add(3);
///     r
/// </code>
/// </example>
let resizeArray = ResizeArrayBuilder()

/// <summary>
/// Builds a finger tree with a computation expression, much like <c>seq { ... }</c>.
/// </summary>
let fingerTree = FingerTreeBuilder()
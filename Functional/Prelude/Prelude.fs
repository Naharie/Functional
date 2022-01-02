[<AutoOpen>]
module Functional.Prelude

#nowarn "44"

// Tuples

/// Groups two values together as a tuple.
let pair a b = a, b
/// Groups three values together as a triple.
let triple a b c = a, b, c

/// <summary>
/// Return the first element of a tuple, <c>fst (a,b) = a</c>.
/// </summary>
let inline _fst tuple = Tuple.TupleInternals.First $ tuple

/// <summary>
/// Return the second element of a tuple, <c>snd (a,b) = b</c>.
/// </summary>
let inline _snd tuple = Tuple.TupleInternals.Second $ tuple

/// A utility function that allows the use of implicit conversion operators.
let inline implicit input = (^b : (static member op_Implicit : ^a -> ^b) input)
/// An operator that allows the use of implicit conversion operators
let inline (~~) input = implicit input
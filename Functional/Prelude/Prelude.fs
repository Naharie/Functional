[<AutoOpen>]
module Function.Prelude

// Tuples

/// Groups two values together as a tuple.
let pair a b = a, b
/// Groups three values together as a triple.
let triple a b c = a, b, c

/// A utility function that allows the use of implicit conversion operators.
let inline implicit input = (^b : (static member op_Implicit : ^a -> ^b) input)
/// An operator that allows the use of implicit conversion operators
let inline (~~) input = implicit input
[<AutoOpen>]
module Functional.Prelude

#nowarn "44"

// Tuples

/// <summary>
/// Groups two values together as a tuple.
/// </summary>
/// <param name="a">The first value to group.</param>
/// <param name="b">The second value to group.</param>
/// <returns>A pair of the two values.</returns>
let inline pair a b = a, b
/// <summary>
/// Groups three values together as a triple.
/// </summary>
/// <param name="a">The first value to group.</param>
/// <param name="b">The second value to group.</param>
/// <param name="c">The third value to group.</param>
/// <returns>A triple containing the three values.</returns>
let inline triple a b c = a, b, c

/// <summary>
/// Return the first element of a tuple, <c>fst (a, b) = a</c>.
/// </summary>
/// <param name="tuple">The tuple to fetch the first item from.</param>
/// <returns>The first item of the specified tuple.</returns>
let inline fst tuple = Tuple.TupleInternals.First $ tuple

/// <summary>
/// Return the second element of a tuple, <c>snd (a, b) = b</c>.
/// </summary>
/// <param name="tuple">The tuple to fetch the second item from.</param>
/// <returns>The second item of the specified tuple.</returns>
let inline snd tuple = Tuple.TupleInternals.Second $ tuple

/// <summary>
/// Calls an available implicit conversion operator on the specified input value.
/// </summary>
/// <param name="input">The value to convert.</param>
/// <returns>The converted value.</returns>
let inline implicit input = (^b : (static member op_Implicit : ^a -> ^b) input)
/// <summary>
/// Calls an available implicit conversion operator on the specified input value.
/// </summary>
/// <param name="input">The value to convert.</param>
/// <returns>The converted value.</returns>
let inline (~~) input = implicit input

/// Throws a generic exception with the message "TODO".
let inline todo<'t> = failwith "TODO" : 't

/// <summary>
/// Creates a tuple from the specified key and value.
/// </summary>
/// <param name="key">The first element of the tuple.</param>
/// <param name="value">The second element of the tuple.</param>
/// <returns>A tuple containing the two elements.</returns>
let inline (=>) key value = key, value
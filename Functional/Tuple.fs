[<RequireQualifiedAccess>]
module Functional.Tuple

open System

[<Obsolete>]
module TupleInternals =
    type First = | First
    with
        static member inline ($) (First, (a, _)) = a
        static member inline ($) (First, (a, _, _)) = a
        static member inline ($) (First, (a, _, _, _)) = a
        static member inline ($) (First, (a, _, _, _, _)) = a
        static member inline ($) (First, (a, _, _, _, _, _)) = a
        static member inline ($) (First, (a, _, _, _, _, _, _)) = a
        static member inline ($) (First, (a, _, _, _, _, _, _, _)) = a

    type Second = | Second
    with
        static member inline ($) (Second, (_, a)) = a
        static member inline ($) (Second, (_, a, _)) = a
        static member inline ($) (Second, (_, a, _, _)) = a
        static member inline ($) (Second, (_, a, _, _, _)) = a
        static member inline ($) (Second, (_, a, _, _, _, _)) = a
        static member inline ($) (Second, (_, a, _, _, _, _, _)) = a
        static member inline ($) (Second, (_, a, _, _, _, _, _, _)) = a
    
    type Map = | Map
    with
        static member inline ($) (Map, (a, b)) = fun f -> (f a), (f b)
        static member inline ($) (Map, (a, b, c)) = fun f -> (f a), (f b), (f c)
        static member inline ($) (Map, (a, b, c, d)) = fun f -> (f a), (f b), (f c), (f d)
        static member inline ($) (Map, (a, b, c, d, e)) = fun f -> (f a), (f b), (f c), (f d), (f e)
        static member inline ($) (Map, (a, b, c, d, e, f')) = fun f -> (f a), (f b), (f c), (f d), (f e), (f f')
        static member inline ($) (Map, (a, b, c, d, e, f', g)) = fun f -> (f a), (f b), (f c), (f d), (f e), (f f'), (f g)
        static member inline ($) (Map, (a, b, c, d, e, f', g, h)) = fun f -> (f a), (f b), (f c), (f d), (f e), (f f'), (f g), (f h)

    type Rev = | Rev
    with
        static member inline ($) (Rev, (a, b)) = b, a
        static member inline ($) (Rev, (a, b, c)) = c, b, a
        static member inline ($) (Rev, (a, b, c, d)) = d, c, b, a
        static member inline ($) (Rev, (a, b, c, d, e)) = e, d, c, b, a
        static member inline ($) (Rev, (a, b, c, d, e, f')) = f', e, d, c, b, a
        static member inline ($) (Rev, (a, b, c, d, e, f', g)) = g, f', e, d, c, b, a
        static member inline ($) (Rev, (a, b, c, d, e, f', g, h)) = h, g, f', e, d, c, b, a

    type Iter = | Iter
    with
        static member inline ($) (Iter, (a, b)) = fun f -> f a; f b
        static member inline ($) (Iter, (a, b, c)) = fun f -> f a; f b; f c
        static member inline ($) (Iter, (a, b, c, d)) = fun f -> f a; f b; f c; f d
        static member inline ($) (Iter, (a, b, c, d, e)) = fun f -> f a; f b; f c; f d; f e
        static member inline ($) (Iter, (a, b, c, d, e, f')) = fun f -> f a; f b; f c; f d; f e; f f'
        static member inline ($) (Iter, (a, b, c, d, e, f', g)) = fun f -> f a; f b; f c; f d; f e; f f'; f g
        static member inline ($) (Iter, (a, b, c, d, e, f', g, h)) = fun f -> f a; f b; f c; f d; f e; f f'; f g; f h

    type Fold = | Fold
    with
        static member inline ($) (Fold, (a, b)) = fun f s -> f (f s a) b
        static member inline ($) (Fold, (a, b, c)) = fun f s -> f (f (f s a) b) c
        static member inline ($) (Fold, (a, b, c, d)) = fun f s -> f (f (f (f s a) b) c) d
        static member inline ($) (Fold, (a, b, c, d, e)) = fun f s -> f (f (f (f (f s a) b) c) d) e
        static member inline ($) (Fold, (a, b, c, d, e, f')) = fun f s -> f (f (f (f (f (f s a) b) c) d) e) f'
        static member inline ($) (Fold, (a, b, c, d, e, f', g)) = fun f s -> f (f (f (f (f (f (f s a) b) c) d) e) f') g
        static member inline ($) (Fold, (a, b, c, d, e, f', g, h)) = fun f s -> f (f (f (f (f (f (f (f s a) b) c) d) e) f') g) h

    type FoldBack = | FoldBack
    with
        static member inline ($) (FoldBack, (a, b)) = fun f s -> s |> f b |> f a
        static member inline ($) (FoldBack, (a, b, c)) = fun f s -> s |> f c |> f b |> f a
        static member inline ($) (FoldBack, (a, b, c, d)) = fun f s -> s |> f d |> f c |> f b |> f a
        static member inline ($) (FoldBack, (a, b, c, d, e)) = fun f s -> s |> f e |> f d |> f c |> f b |> f a
        static member inline ($) (FoldBack, (a, b, c, d, e, f')) = fun f s -> s |> f f' |> f e |> f d |> f c |> f b |> f a
        static member inline ($) (FoldBack, (a, b, c, d, e, f', g)) = fun f s -> s |> f g |> f f' |> f e |> f d |> f c |> f b |> f a
        static member inline ($) (FoldBack, (a, b, c, d, e, f', g, h)) = fun f s -> s |> f h |> f g |> f f' |> f e |> f d |> f c |> f b |> f a

#nowarn "44"

/// <summary>
/// Applies the specified mapping function to each element of the tuple.
/// </summary>
/// <param name="mapping">The function to apply to each item.</param>
/// <param name="tuple">The tuple containing the items to have the function applied to.</param>
/// <returns>A new tuple containing the results of the mapping function.</returns>
let inline map (mapping: 'a -> 'b) tuple = (TupleInternals.Map $ tuple) mapping
/// <summary>
/// Reverses the order of the elements in the specified tuple.
/// </summary>
/// <param name="tuple">The tuple to reverse.</param>
/// <returns>A tuple containing all the elements of the input tuple in reverse order.</returns>
let inline rev tuple = TupleInternals.Rev $ tuple
/// <summary>
/// Applies <paramref name="action"/> to each element of the tuple.
/// </summary>
/// <param name="action">The function to apply to the tuple's contents.</param>
/// <param name="tuple">The tuple to iterate over.</param>
let inline iter action tuple = (TupleInternals.Iter $ tuple) action
/// <summary>
/// Applies a function to each element of the tuple, threading an accumulator argument through the computation. 
/// </summary>
/// <param name="folder">The function to apply to pairing of state and item.</param>
/// <param name="state">The initial starting state.</param>
/// <param name="tuple">The tuple to iterate over.</param>
/// <returns>The resulting state generated by <paramref name="folder"/>.</returns>
let inline fold folder state tuple = (TupleInternals.Fold $ tuple) folder state
/// <summary>
/// Applies a function to each element of the tuple iterating in reverse order, threading an accumulator argument through the computation. 
/// </summary>
/// <param name="folder">The function to apply to pairing of state and item.</param>
/// <param name="state">The initial starting state.</param>
/// <param name="tuple">The tuple to iterate over.</param>
/// <returns>The resulting state generated by <paramref name="folder"/>.</returns>
let inline foldBack folder tuple state = (TupleInternals.FoldBack $ tuple) folder state
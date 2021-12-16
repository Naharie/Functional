[<AutoOpen>]
module Functional.LiftedFunctions

let (.&&) a b = fun x -> a x && b
let (.||) a b = fun x -> a x || b

let (&&.) a b = fun x -> a && b x
let (||.) a b = fun x -> a || b x

let (.&&.) a b = fun x -> a x && b x
let (.||.) a b = fun x-> a x || b x

let liftL f a b = fun x -> f (a x) b
let liftR f a b = fun x -> f a (b x)
let lift2 f a b = fun x -> f (a x) (b x)

/// Flips the order of the first two arguments taken by a function.
let (!^) f = fun a b -> f b a
/// Flips the order of the first two arguments taken by a function.
let flip f = fun a b -> f b a
/// Reverses the order of the first three arguments taken by a function.
let flip3 f = fun a b c -> f c b a

let (.<) a b = liftL (<) a b
let (.>) a b = liftL (>) a b
let (.<=) a b = liftL (<=) a b
let (.>=) a b = liftL (>=) a b
let (.=) a b = liftL (=) a b
let (.<>) a b = liftL (<>) a b

let (<.) a b = liftR (<) a b
let (>.) a b = liftR (>) a b
let (<=.) a b = liftR (<=) a b
let (>=.) a b = liftR (>=) a b
let (=.) a b = liftR (=) a b
let (<>.) a b = liftR (<>) a b

let (.<.) a b = lift2 (<) a b
let (.>.) a b = lift2 (>) a b
let (.<=.) a b = lift2 (<=) a b
let (.>=.) a b = lift2 (>=) a b
let (.=.) a b = lift2 (=) a b
let (.<>.) a b = lift2 (<>) a b
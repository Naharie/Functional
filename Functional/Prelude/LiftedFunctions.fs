[<AutoOpen>]
module Functional.LiftedFunctions

let liftL f a b = fun x -> f (a x) b
let liftR f a b = fun x -> f a (b x)

let lift2 f a b = fun x y -> f (a x) (b y)
let lift2R f b a = fun x y -> f (a x) (b y)

let lift2S f a b = fun x -> f (a x) (b x)
let lift2RS f b a = fun x -> f (a x) (b x) 

/// Flips the order of the first two arguments taken by a function.
let (!^) f = fun a b -> f b a
/// Flips the order of the first two arguments taken by a function.
let flip f = fun a b -> f b a
/// Reverses the order of the first three arguments taken by a function.
let flip3 f = fun a b c -> f c b a

// Equality

let (.&&) a b = fun x -> a x && b
let (.||) a b = fun x -> a x || b

let (&&.) a b = fun x -> a && b x
let (||.) a b = fun x -> a || b x

let (.&&.) a b = fun x y -> a x && b x y
let (.||.) a b = fun x y -> a x || b y

let (/&&/) a b = fun x -> a x && b x
let (/||/) a b = fun x -> a x || b x

// Comparision

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

let (/</) a b = lift2S (<) a b
let (/>/) a b = lift2S (>) a b
let (/<=/) a b = lift2S (<=) a b
let (/>=/) a b = lift2S (>=) a b
let (/=/) a b = lift2S (=) a b
let (/<>/) a b = lift2S (<>) a b

// Math

let (.+) a b = liftL (+) a b
let (.-) a b = liftL (-) a b
let (.*) a b = liftL (*) a b
let (./) a b = liftL (/) a b

let (+.) a b = liftR (+) a b
let (-.) a b = liftR (-) a b
let (/*.) a b = liftR (*) a b
let (/.) a b = liftR (/) a b

let (.+.) a b = lift2 (+) a b
let (.-.) a b = lift2 (-) a b
let (.*.) a b = lift2 (*) a b
let (./.) a b = lift2 (/) a b

let (/+/) a b = lift2S (+) a b
let (/-/) a b = lift2S (-) a b
let (/*/) a b = lift2S (*) a b
let (/././) a b = lift2S (/) a b
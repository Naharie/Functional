[<Microsoft.FSharp.Core.AutoOpen>]
module Functional.LiftedFunctions

// Math

let (/+) f a b = (f b) + a
let (+/) y a b = y + (a b)
let (/+/) a b x = (a x) + (b x)

let (/-) f a b = (f b) - a
let (-/) y a b = y - (a b)
let (/-/) a b x = (a x) - (b x)

let (/*) f a b = (f b) * a
let ( */) y a b = y * (a b)
let (/*/) a b x = (a x) * (b x)

let (/@) f a b = (f b) / a
let (@/) y a b = y / (a b)
let (/@/) a b x = (a x) / (b x)

// Comparision

let (/=) f a b = (f b) = a
let (=/) y a b = y = (a b)
let (/=/) a b x = (a x) = (b x)

let (/<>) f a b = (f b) <> a
let (<>/) y a b = y <> (a b)
let (/<>/) a b x = (a x) <> (b x)

let (/<) f a b = (f b) < a
let (</) y a b = y < (a b)
let (/</) a b x = (a x) < (b x)

let (/<=) f a b = (f b) <= a
let (<=/) y a b = y <= (a b)
let (/<=/) a b x = (a x) <= (b x)

let (/>) f a b = (f b) > a
let (>/) y a b = y > (a b)
let (/>/) a b x = (a x) > (b x)

let (/>=) f a b = (f b) >= a
let (>=/) y a b = y >= (a b)
let (/>=/) a b x = (a x) >= (b x)
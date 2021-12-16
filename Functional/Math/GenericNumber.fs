module Functional.NumericLiteralG

let inline FromZero() = LanguagePrimitives.GenericZero
let inline FromOne() = LanguagePrimitives.GenericOne

let inline FromInt32 number =
    let rec build total number =
        match number with
        | 0 -> total
        | n when n < 0 ->
            total - LanguagePrimitives.GenericOne
        | n when n > 0 ->
            total + LanguagePrimitives.GenericOne

        // Only here to making the compiler happy.
        | _ -> total

    build LanguagePrimitives.GenericZero number

let inline FromInt64 number =
    let rec build total number =
        match number with
        | 0L -> total
        | n when n < 0L ->
            total - LanguagePrimitives.GenericOne
        | n when n > 0L ->
            total + LanguagePrimitives.GenericOne

        // Only here to making the compiler happy.
        | _ -> total

    build LanguagePrimitives.GenericZero number
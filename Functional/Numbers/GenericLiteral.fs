module Functional.NumericLiteralG

let inline FromZero() = LanguagePrimitives.GenericZero
let inline FromOne() = LanguagePrimitives.GenericOne

let inline FromInt32 number =
    let rec build total number =
        match number with
        | 0 -> total
        | n when n < 0 ->
            build (total - LanguagePrimitives.GenericOne) (number + 1)
        | n when n > 0 ->
            build (total + LanguagePrimitives.GenericOne) (number - 1)

        // Only here to make the compiler happy.
        | _ -> total

    build LanguagePrimitives.GenericZero number

let inline FromInt64 number =
    let rec build total number =
        match number with
        | 0L -> total
        | n when n < 0L ->
            build (total - LanguagePrimitives.GenericOne) (number + 1L)
        | n when n > 0L ->
            build (total + LanguagePrimitives.GenericOne) (number - 1L)

        // Only here to make the compiler happy.
        | _ -> total

    build LanguagePrimitives.GenericZero number
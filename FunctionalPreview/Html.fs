module FunctionalPreview.Html

open Functional

/// Escapes html code in the given string.
let escape text =
    text
    |> String.replaceAll [
        "&", "&amp;"
        "<", "&lt;"
        ">", "&gt;"
        "\"", "&quot;"
        "'", "&#39;"
    ]

type HtmlPiece =
    | Element of name:string * children:HtmlPiece list
    | Text of string
    | Attribute of name:string * value:string
with
    override this.ToString () =
        match this with
        | Element(name, children) ->
            failwith "TODO"
        
        | Text text -> escape text
        | Attribute _ -> ""
module FunctionalPreview.Html

open Functional

type HtmlPiece =
    | Element of name:string * children:HtmlPiece list
    | Text of string
    | Attribute of name:string * value:string
with
    member this.ToString () =
        match this with
        | Element(name, children) ->
            failwith "TODO"
        
        | Text text ->
            text
            |> String.replaceAll [
                "&", "&amp;"
                "<", "&lt;"
                ">", "*gt;"
            ]
        
        | Attribute _ -> ""
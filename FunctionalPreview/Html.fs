module Functional.Preview.Html

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

type Html =
    | SelfClosing of name:string
    | Element of name:string * children:Html list
    | Text of string
    | Raw of string
    | Attribute of name:string * value:string
with
    override this.ToString () =
        match this with
        | SelfClosing name -> $"<{name} />"
        | Element(name, children) ->
            let attributes =
                children
                |> List.choose (function
                    | Attribute (name, value) ->
                        if value <> "" then
                            let value =
                                value
                                |> String.trim
                                |> String.replaceAll [ "\r", ""; "\n", " " ]

                            $"{name}=\"{escape value}\""
                        else
                            name
                        |> Some
                    | _ -> None
                )
                |> String.concat " "
                |> fun x -> if x <> "" then " " + x else ""
            let children =
                [
                    for child in children do
                        let html = string child
                        if html <> "" then yield html
                ]
                |> String.concat ""
                
            $"<{name}{attributes}>{children}</{name}>"
        
        | Text text -> escape text
        | Raw text -> text
        | Attribute _ -> ""

[<AutoOpen>]
module Elements =
    let private make name children = Element(name, children)

    let text = Text
    let raw = Raw

    let script contents = Element("script", [ Raw contents ])

    let div = make "div"
    let span = make "span"
    
    let b = make "b"
    let i = make "i"
    
    let table = make "table"
    let thead = make "thead"
    let tbody = make "tbody"
    let tr = make "tr"
    let td = make "td"
    
[<AutoOpen>]
module Attributes =
    let private make name value = Attribute(name, value)
    
    let style = String.trim >> make "style"
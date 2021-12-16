[<AutoOpen>]
module Functional.Scanf

open System
open System.Text
open System.Text.RegularExpressions
open FSharp.Reflection

[<RequireQualifiedAccess>]
type private ParserType =
    | Int
    | Float
    | Boolean
    | String
    | Character
with
    member this.AsRegex =
        match this with
        | Int -> "(-?\d+)"
        | Float -> "(-?\d+(?:\.\d+)?)"
        | Boolean -> "(true|false)"
        | String -> "(.+)"
        | Character -> "(.)"

type private PrintChunk =
    | Literal of string
    | Parser of ParserType * parser:(string -> obj)

let private parsers = dict [
    'b', (fun (value: string) -> value.ToUpper() = "TRUE") >> box
    'i', int >> box
    'f', float >> box
    's', box
    'c', char >> box
]

let private getChunks (format: string) =
    let rec loop chunks literal format =
        match format with
        | '%' :: '%' :: format ->
            loop chunks ('%' :: literal) format
        | '%' :: f :: format ->
            let chunks =
                match literal with
                | [] -> chunks
                | _ ->
                    let chunk =
                        literal
                        |> Array.ofList
                        |> Array.rev
                        |> String

                    (Literal chunk) :: chunks

            if parsers.ContainsKey f then
                let parserType =
                    match f with
                    | 'i' -> ParserType.Int
                    | 'f' -> ParserType.Float
                    | 'b' -> ParserType.Boolean
                    | 's' -> ParserType.String
                    | 'c' -> ParserType.Character
                    | _ -> failwithf "Unkown parser: %%%c" f

                let chunk = Parser (parserType, parsers.[f])

                loop (chunk :: chunks) [] format
            else
                failwithf "Unknown parser: %%%c" f

        | x :: xs ->
            loop chunks (x :: literal) xs

        | [] ->
            match literal with
            | [] ->
                chunks
                |> List.rev
            | _ ->
                let chunk =
                    literal
                    |> Array.ofList
                    |> Array.rev
                    |> String

                (Literal chunk :: chunks) |> List.rev

    loop [] [] (Array.toList (format.ToCharArray()))

let removeExtraWhitespace (text: string) =
    match text with
    | null -> ""
    | _ ->
        let builder = StringBuilder text.Length

        for index in 0..text.Length - 1 do
            let character = text.[index]

            if index = 0 || character <> ' ' || (character = ' ' && text.[index - 1] <> ' ') then
                builder.Append character |> ignore

        string builder

let scanf (format: PrintfFormat<_,_,_,_,'t>) text : 't =
    let chunks = getChunks format.Value
    let parsers =
        chunks
        |> List.choose (function
            | Parser (_, func) -> Some func
            | _ -> None
        )
        |> Array.ofList
    let pattern =
      chunks
      |> List.map (function
          | Literal text -> Regex.Escape text
          | Parser (parserType, _) -> parserType.AsRegex
      )
      |> String.concat ""

    let regex = Regex pattern
    let result = regex.Match text

    let values =
        result.Groups
        |> Seq.cast<Group>
        |> Seq.tail
        |> Array.ofSeq
        |> Array.zip parsers
        |> Array.map (fun (parser, group) ->
            parser group.Value
        )
    
    match values with
    | [| value |] ->
        value :?> 't
    | _ ->
        FSharpValue.MakeTuple(values, typeof<'t>) :?> 't
let scanwf format text =
    text
    |> removeExtraWhitespace
    |> scanf format

let (|Scanf|_|) (format: PrintfFormat<_,_,_,_,'t>) (text: string) =
    let chunks = getChunks format.Value
    let parsers =
        chunks
        |> List.choose (function
            | Parser (_, func) -> Some func
            | _ -> None
        )
        |> Array.ofList
    let pattern =
      chunks
      |> List.map (function
          | Literal text -> Regex.Escape text
          | Parser (parserType, _) -> parserType.AsRegex
      )
      |> String.concat ""

    let result = Regex.Match (text, pattern)

    if result.Success then
        let groups =
            result.Groups
            |> Seq.cast<Group>
            |> Seq.tail
            |> Array.ofSeq

        if groups.Length = parsers.Length then
            let values =
                groups
                |> Array.zip parsers
                |> Array.map (fun (parser, group) ->
                    parser group.Value
                )

            match values with
            | [| value |] ->
                Some (value :?> 't)
            | _ ->
                Some (FSharpValue.MakeTuple(values, typeof<'t>) :?> 't)
        else
            None
    else
        None
let (|Scanwf|_|) format text =
    ``|Scanf|_|`` format (removeExtraWhitespace text)
namespace Functional

open System.Text.RegularExpressions
open FSharp.Reflection

[<RequireQualifiedAccess>]
module Regex =
    let isMatch (pattern: string) (text: string) =
        Regex.IsMatch(text, pattern)

    let matches (pattern: string) (text: string) =
        let matches = Regex.Matches(text, pattern)

        matches
        |> Seq.cast<Match>
        |> Seq.map (_.Value)
        |> Seq.toList

    let groups (pattern: string) (text: string) =
        let match' = Regex.Match(text, pattern)

        if match'.Success then
            match'.Groups
            |> Seq.cast<Group>
            |> Seq.tail
            |> Seq.map (fun group -> group.Name, group.Value)
            |> Map.ofSeq
        else
            Map.empty
    let groupValues (pattern: string) (text: string) =
        let match' = Regex.Match (text, pattern)

        if match'.Success then
            match'.Groups
            |> Seq.cast<Group>
            |> Seq.tail
            |> Seq.map (_.Value)
            |> Seq.toList
        else
            []

    let escape (text: string) = Regex.Escape text

[<AutoOpen>]
module RegexPrelude =
    let (|Regex|_|) (pattern: string) (text: string) =
        let values =
            Regex.groupValues pattern text
            |> List.map box
            |> List.toArray

        match values with
        | [||] -> None
        | _ ->
            try
                let convertValue value element =
                    if element = typeof<int> then
                        value |> unbox<string> |> int32 |> box
                    elif element = typeof<float> then
                        value |> unbox<string> |> float |> box
                    elif element = typeof<float> then
                        match unbox<string> value with
                        | "true" -> box true
                        | _ -> box false
                    else
                        value

                match values with
                | [| value |] ->
                    Some (convertValue value typeof<'t> :?> 't)
                | _ ->
                    let elements = FSharpType.GetTupleElements typeof<'t>
                    let values =
                        Array.zip values elements
                        |> Array.map (fun (value, element) ->
                            convertValue value element
                        )

                    Some (FSharpValue.MakeTuple (values, typeof<'t>) :?> 't)
            with
            | _ ->
                printfn $"Failed to parse %s{text} with regex %s{pattern}."
                printfn $"Values: %A{values}"

                reraise()
namespace Functional

open System.Text.RegularExpressions
open FSharp.Reflection

[<RequireQualifiedAccess>]
module Regex =
    /// <summary>
    /// Determines whether the specified text matches the specified regex.
    /// </summary>
    /// <param name="pattern">The regex pattern to test with.</param>
    /// <param name="text">The text to test against the regex.</param>
    /// <returns>Whether the specified text matches the specified regex.</returns>
    let isMatch (pattern: string) (text: string) =
        Regex.IsMatch(text, pattern)

    /// <summary>
    /// Returns a list of all matches of <paramref name="pattern"/> in the input string.
    /// </summary>
    /// <param name="pattern">The pattern to find matches for.</param>
    /// <param name="text">The text to find matches in.</param>
    /// <returns>A list of all matches of <paramref name="pattern"/> in the input string.</returns>
    let matches (pattern: string) (text: string) =
        let matches = Regex.Matches(text, pattern)

        matches
        |> Seq.cast<Match>
        |> Seq.map (_.Value)
        |> Seq.toList

    /// <summary>
    /// Returns a map containing every named group in the regex and it's value if the regex matched the input string.
    /// </summary>
    /// <param name="pattern">The regex to test against the input string.</param>
    /// <param name="text">The text to be matched against by the regex.</param>
    /// <returns>A map containing every named group in the regex and it's value if the regex matched the input string.</returns>
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
    /// <summary>
    /// Returns a list containing the values every group in the regex if it matches the input string.
    /// </summary>
    /// <param name="pattern">The regex to test against the input string.</param>
    /// <param name="text">The text to be matched against by the regex.</param>
    /// <returns>A list containing the values every group in the regex if it matches the input string.</returns>
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

    /// <summary>
    /// Escapes all special characters in a string so that the resulting regex will match the original string, instead of the string acting as a regex.
    /// </summary>
    /// <param name="text">The text to escape.</param>
    /// <returns>The escaped string as a regex.</returns>
    let escape (text: string) = Regex.Escape text

[<AutoOpen>]
module RegexPrelude =
    /// <summary>
    /// Attempts to match the specified text with the specified regex.
    /// </summary>
    /// <param name="pattern">The regex to test with.</param>
    /// <param name="text">The text to test against.</param>
    /// <returns>A list containing the matched groups.</returns>
    let (|Regex|_|) (pattern: string) (text: string) =
        let values =
            Regex.groupValues pattern text
            |> List.map box
            
        match values with
        | [] -> None
        | _ -> Some values
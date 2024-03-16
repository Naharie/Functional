open System
open System.IO
open Functional

let path = "/home/naharie/Downloads/expected.txt"
let lines = File.ReadAllLines path

let getNumbers line =
    Regex.matches "-?\d+" line
    |> List.map int

let partsOfSpeech = [
    "N"
    "PRON"
    "PACK"
    "ADJ"
    "NUM"
    "ADV"
    "V"
    "VPAR"
    "SUPINE"
    "PREP"
    "CONJ"
    "INTERJ"
]

let groups =
    lines
    |> Array.filter ((<>) "=>")
    |> Array.chunkBy2 (fun line ->
        String.isNullOrEmpty line
        || line.Contains ";"
    )
    |> Array.map (Array.filter (String.isNullOrEmpty >> not))
    |> Array.filter (fun group -> group.Length >= 2)
    
    |> Array.map (fun group ->
        let parts = group[0].Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        
        let partOfSpeech = parts[1]
        let numbers = getNumbers group[0]
        
        let chunk =
            match numbers with
            | a :: b :: _ -> partOfSpeech, a, b
            | _ -> partOfSpeech, 0, 0

        chunk, group
    )

let shrunk =
    groups
    |> Array.distinctBy fst
    |> Array.sortBy fst
    |> Array.map snd
 
printfn $"%i{groups.Length} - %i{shrunk.Length}"

let text =
    shrunk
    |> Array.map (String.concat "\r\n")
    |> String.concat "\r\n\r\n"
    
File.WriteAllText(path, text)
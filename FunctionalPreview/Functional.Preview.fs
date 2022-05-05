[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Functional.Preview

open System
open System.Collections
open Functional.Preview
open Microsoft.DotNet.Interactive.Formatting
open Html

let private labelledBlock label value (writer: IO.TextWriter) =
    div [
        style """
            display: inline-block;
            padding: 5px;
            border-radius: 5px;
            background-color: lightgray;
        """
        span [ style "color: #303030;"; text (label + ": ") ]
        span [ style "color: #1171c2;"; value  ]
    ]
    |> string
    |> writer.Write

let private registerPrimitives () =
    // DateTime
    
    Formatter.Register<DateTime>((fun (date: DateTime) ->
        labelledBlock "Date" (date.ToString "ddd, MMM d, yyyy, H:mm" |> text)
    ), "text/html")
    
    Formatter.Register<DateTimeOffset>((fun (offset: DateTimeOffset) ->
        labelledBlock "Date" (offset.ToString "ddd, MMM d, yyyy H:mm" |> text)
    ), "text/html")
    
    // Html
    
    Formatter.Register<Html>(fun value (writer: IO.TextWriter) ->
        writer.Write (string value)
    )

let private registerCollections () =
    let enumerableFormatter (enumerable: IEnumerable) (writer: IO.TextWriter) =
        table [
            thead [ td []; td [ text "Item" ] ]
            
            let enumerable =
                enumerable
                |> Seq.cast<obj>
                |> Seq.indexed
                |> Seq.truncate Formatter.ListExpansionLimit
            
            for index, value in enumerable do
                tr [
                    td [ text (string index) ]
                    td [ raw (Formatter.ToDisplayString(value, "text/html")) ]
                ]
        ]
        |> string
        |> writer.Write
        
    let nestedArrays (value: Array) (writer: IO.TextWriter) =
        table [
            let rowLength =
                seq {
                    for i in 0..value.Length - 1 do
                        (value.GetValue i :?> Array).Length
                }
                |> Seq.max

            thead [ for i in 0..rowLength - 1 do td [ text $"Item {i}" ] ]
            tbody [
                for i in 0..(min Formatter.ListExpansionLimit (value.Length - 1)) do
                    let row = value.GetValue i :?> Array
                    
                    tr [
                        for j in 0..row.Length - 1 do
                            td [ raw (row.GetValue(j).ToDisplayString "text/html") ]
                        
                        for _ in row.Length..rowLength - 1 do td []
                    ]
            ]
        ]
        |> string
        |> writer.Write
    let squareArray (value: Array) (writer: IO.TextWriter) =
        table [
            let length = value.GetUpperBound 0
            let rowLength = value.GetUpperBound 1
            
            thead [ for i in 0..rowLength do td [ text $"Item {i}" ] ]
            tbody [
                for i in 0..(min Formatter.ListExpansionLimit length) do
                    tr [
                        for j in 0..rowLength do
                            td [ raw(value.GetValue(i, j).ToDisplayString "text/html") ]
                    ]
            ]
        ]
        |> string
        |> writer.Write
        
    let formatEnumerable (enumerable: IEnumerable) (writer: IO.TextWriter) =
        if enumerable.GetType().IsArray then
            let value = enumerable :?> Array
            
            if value.Rank > 2 then
                enumerableFormatter value writer
            elif value.Rank = 2 then
                squareArray value writer
            elif (let valueType = value.GetType().GetElementType() in valueType.IsArray) then
                nestedArrays value writer
            else
                enumerableFormatter value writer
        else
            enumerableFormatter enumerable writer
    
    Formatter.Register<IEnumerable> enumerableFormatter
    Formatter.Register(typedefof<seq<_>>, (fun (value: obj) ->
        formatEnumerable (value :?> IEnumerable)
    ), "text/html")
    
    // TODO: Proper tuple support.
    
let register () =
    registerPrimitives()
    registerCollections()
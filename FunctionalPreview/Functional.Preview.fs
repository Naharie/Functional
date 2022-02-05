module Functional.Preview

open System
open Microsoft.DotNet.Interactive.Formatting

let private labelledText label text (writer: IO.TextWriter) =
    writer.Write($"""
        <div style="display: inline-block; padding: 5px; border-radius: 5px; background-color: lightgray;">
            <span style="color: #303030;">%s{label}: </span>
            <span style="color: #1171c2;">%s{text}</span>
        </div>
    """)

let private registerPrimitives () =
    Formatter.Register<DateTime>((fun (date: DateTime) ->
        labelledText "Date" (date.ToString "ddd, MMM d, yyyy, H:mm")
    ), "text/html")
    
    Formatter.Register<DateTimeOffset>((fun (offset: DateTimeOffset) ->
        labelledText "Date" (offset.ToString "ddd, MMM d, yyyy H:mm")
    ), "text/html")
    
let private registerCollections () =
    let enumerableFormatter =
        Formatter.RegisteredFormatters true
        |> Seq.find (fun formatter ->
            formatter.MimeType = "text/html"
            && formatter.Type.FullName = "System.Collections.IEnumerable"
        )
        
    let nestedArrays (value: Array) (writer: IO.TextWriter) =
        writer.Write "<table>"
            
        let rowLength =
            seq {
                for i in 0..value.Length - 1 do
                    (value.GetValue i :?> Array).Length
            }
            |> Seq.max
            
        writer.Write "<thead>"
            
        for i in 0..rowLength - 1 do
            writer.Write $"<td>Item {i + 1}</td>"
            
        writer.Write "</thead>"
        writer.Write "<tbody>"

        for i in 0..(min Formatter.ListExpansionLimit (value.Length - 1)) do
            let row = value.GetValue i :?> Array
            
            writer.Write "<tr>"

            for j in 0..row.Length - 1 do
                writer.Write "<td>"
                writer.Write (row.GetValue(j).ToDisplayString "text/html")
                writer.Write "</td>"
                
            for j in row.Length..rowLength - 1 do
                writer.Write "<td></td>"
                
            writer.Write "</tr>"

        writer.Write "</tbody>"
        writer.Write "</table>"
    let squareArray (value: Array) (writer: IO.TextWriter) =
        writer.Write "<table>"
        writer.Write "<thead>"
            
        let length = value.GetUpperBound 0
        let rowLength = value.GetUpperBound 1
            
        for i in 0..rowLength do
            writer.Write $"<td>Item {i + 1}</td>"
            
        writer.Write "</thead>"
        writer.Write "<tbody>"

        for i in 0..(min Formatter.ListExpansionLimit length) do 
            writer.Write "<tr>"

            for j in 0..rowLength do
                writer.Write "<td>"
                writer.Write (value.GetValue(i, j).ToDisplayString "text/html")
                writer.Write "</td>"
                
            writer.Write "</tr>"

        writer.Write "</tbody>"
        writer.Write "</table>"
    
    Formatter.Register<Array>((fun (value: Array) (writer: IO.TextWriter) ->
        if value.Rank > 2 then
            enumerableFormatter.Format(value, writer)
        elif value.Rank = 2 then
            squareArray value writer
        elif (let valueType = value.GetType().GetElementType() in valueType.IsArray) then
            nestedArrays value writer
        else
            enumerableFormatter.Format(value, writer)
    ), "text/html")
    
let register () =
    registerPrimitives()
    registerCollections()
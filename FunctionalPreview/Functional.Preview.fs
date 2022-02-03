module Functional.Preview

open System
open System.Threading.Tasks
open Microsoft.DotNet.Interactive
open Microsoft.DotNet.Interactive.Formatting

let register () =
    Formatter.Register<DateTime>((fun (date: DateTime) (writer: IO.TextWriter) ->
        writer.Write($"""
            <div style="display: inline-block; padding: 5px; border-radius: 5px; background-color: lightgray;">
                <span style="color: #303030;">Day: </span>
                <span style="color: #1171c2;">{date.ToString "ddd, MMM d, yyyy"}</span>
            </div>
        """)
    ), "text/html")
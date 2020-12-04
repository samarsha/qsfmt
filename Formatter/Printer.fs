module internal QsFmt.Formatter.Printer

open System

open QsFmt.Formatter.SyntaxTree

let private printTrivia =
    function
    | Whitespace ws -> Whitespace.toString ws
    | NewLine -> Environment.NewLine
    | Comment comment -> Comment.toString comment

let private printPrefix = List.map printTrivia >> String.concat ""

let printer =
    { new Reducer<_>() with
        override _.Combine(x, y) = x + y

        override _.Terminal terminal =
            printPrefix terminal.Prefix + terminal.Text }

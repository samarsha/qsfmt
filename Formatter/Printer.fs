module internal QsFmt.Formatter.Printer

#nowarn "40"

open System

open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Reducer

let private printTrivia =
    function
    | Whitespace ws -> Whitespace.toString ws
    | NewLine -> Environment.NewLine
    | Comment comment -> Comment.toString comment

let private printPrefix = List.map printTrivia >> String.concat ""

let printer =
    { new Reducer<_>() with
        override _.Combine(x, y) = x + y

        // TODO: Remove this when callable declarations contain the argument tuple.
        override reducer.CallableDeclaration callable =
            reducer.Terminal callable.CallableKeyword
            + reducer.Terminal callable.Name
            + "()"
            + reducer.Terminal callable.Colon
            + reducer.Type callable.ReturnType
            + reducer.Block(reducer.Statement, callable.Block)

        override _.Terminal terminal =
            printPrefix terminal.Prefix + terminal.Text }

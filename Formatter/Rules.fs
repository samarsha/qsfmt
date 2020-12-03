module internal QsFmt.Formatter.Rules

#nowarn "40"

open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Rewriter
open QsFmt.Formatter.SyntaxTree.Statement

let rec private mapWithPrevious previous f =
    function
    | [] -> []
    | x :: xs -> f previous x :: mapWithPrevious (Some x) f xs

let collapseSpaces =
    { new Rewriter<_>() with
        override _.Terminal () terminal =
            let prefix =
                terminal.Prefix
                |> mapWithPrevious None (fun previous trivia ->
                       match previous, trivia with
                       | Some NewLine, Whitespace _ -> trivia
                       | _, Whitespace _ -> Trivia.collapseSpaces trivia
                       | _ -> trivia)

            { terminal with Prefix = prefix } }

let singleSpaceAfterLetBinding =
    { new Rewriter<_>() with
        override _.Let () lets =
            let equals =
                { lets.Equals with
                      Prefix = [ Trivia.spaces 1 ] }

            { lets with Equals = equals } }

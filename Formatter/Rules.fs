module internal QsFmt.Formatter.Rules

#nowarn "40"

open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Rewriter
open QsFmt.Formatter.SyntaxTree.Statement

let rec private collectWithAdjacent before mapping =
    function
    | [] -> []
    | [ x ] -> mapping before x None
    | x :: y :: rest ->
        mapping before x (Some y)
        @ collectWithAdjacent (Some x) mapping (y :: rest)

let private collapseTriviaSpaces previous trivia _ =
    match previous, trivia with
    | Some NewLine, Whitespace _ -> [ trivia ]
    | _, Whitespace _ -> [ Trivia.collapseSpaces trivia ]
    | _ -> [ trivia ]

let collapseSpaces =
    { new Rewriter<_>() with
        override _.Terminal () terminal =
            { terminal with
                  Prefix =
                      terminal.Prefix
                      |> collectWithAdjacent None collapseTriviaSpaces } }

let singleSpaceAfterLetBinding =
    { new Rewriter<_>() with
        override _.Let () lets =
            let equals =
                { lets.Equals with
                      Prefix = [ Trivia.spaces 1 ] }

            { lets with Equals = equals } }

let private indentTrivia level previous trivia after =
    match previous, trivia, after with
    | Some NewLine, Whitespace _, _ -> [ Trivia.spaces (4 * level) ]
    | _, NewLine, Some (Comment _)
    | _, NewLine, None -> [ NewLine; Trivia.spaces (4 * level) ]
    | _ -> [ trivia ]

let private indentTerminal level terminal =
    { terminal with
          Prefix =
              terminal.Prefix
              |> collectWithAdjacent None (indentTrivia level) }

let indent =
    { new Rewriter<_>() with
        override rewriter.Namespace level ns =
            { ns with
                  NamespaceKeyword = indentTerminal level ns.NamespaceKeyword
                  Elements =
                      ns.Elements
                      |> List.map (level + 1 |> rewriter.NamespaceElement) }

        override rewriter.CallableDeclaration level callable =
            { callable with
                  CallableKeyword = indentTerminal level callable.CallableKeyword
                  Statements =
                      callable.Statements
                      |> List.map (level + 1 |> rewriter.Statement)
                  CloseBrace = indentTerminal level callable.CloseBrace }

        override _.Let level lets =
            { lets with
                  LetKeyword = indentTerminal level lets.LetKeyword }

        override _.Return level returns =
            { returns with
                  ReturnKeyword = indentTerminal level returns.ReturnKeyword } }

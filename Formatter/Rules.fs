module internal QsFmt.Formatter.Rules

#nowarn "40"

open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Formatter.SyntaxTree.Rewriter
open QsFmt.Formatter.SyntaxTree.Statement

let private collectWithAdjacent =
    let rec withBefore before mapping =
        function
        | [] -> []
        | [ x ] -> mapping before x None
        | x :: y :: rest ->
            mapping before x (Some y)
            @ withBefore (Some x) mapping (y :: rest)

    withBefore None

let private collapseTriviaSpaces previous trivia _ =
    match previous, trivia with
    | Some NewLine, Whitespace _ -> [ trivia ]
    | _, Whitespace _ -> [ Trivia.collapseSpaces trivia ]
    | _ -> [ trivia ]

let collapsedSpaces =
    { new Rewriter<_>() with
        override _.Terminal((), terminal) =
            terminal
            |> Terminal.mapPrefix (collectWithAdjacent collapseTriviaSpaces) }

let operatorSpacing =
    { new Rewriter<_>() with
        override _.Let((), lets) =
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

let private indentTerminal level =
    Terminal.mapPrefix (indentTrivia level |> collectWithAdjacent)

let indentation =
    { new Rewriter<_>() with
        override rewriter.Namespace(level, ns) =
            { ns with
                  NamespaceKeyword = indentTerminal level ns.NamespaceKeyword
                  Elements =
                      ns.Elements
                      |> List.map (fun element -> rewriter.NamespaceElement(level + 1, element)) }

        override rewriter.CallableDeclaration(level, callable) =
            { callable with
                  CallableKeyword = indentTerminal level callable.CallableKeyword
                  Statements =
                      callable.Statements
                      |> List.map (fun statement -> rewriter.Statement(level + 1, statement))
                  CloseBrace = indentTerminal level callable.CloseBrace }

        override rewriter.Statement(level, statement) =
            base.Statement(level, statement)
            |> Statement.mapPrefix (indentTrivia level |> collectWithAdjacent)

        override rewriter.If(level, ifs) =
            { ifs with
                  IfKeyword = indentTerminal level ifs.IfKeyword
                  Statements =
                      ifs.Statements
                      |> List.map (fun statement -> rewriter.Statement(level + 1, statement))
                  CloseBrace = indentTerminal level ifs.CloseBrace }

        override rewriter.Else(level, elses) =
            { elses with
                  ElseKeyword = indentTerminal level elses.ElseKeyword
                  Statements =
                      elses.Statements
                      |> List.map (fun statement -> rewriter.Statement(level + 1, statement))
                  CloseBrace = indentTerminal level elses.CloseBrace } }

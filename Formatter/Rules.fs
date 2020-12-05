module internal QsFmt.Formatter.Rules

#nowarn "40"

open QsFmt.Formatter.SyntaxTree

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

let private indentPrefix level =
    let indentTrivia previous trivia after =
        match previous, trivia, after with
        | Some NewLine, Whitespace _, _ -> [ Trivia.spaces (4 * level) ]
        | _, NewLine, Some (Comment _)
        | _, NewLine, None -> [ NewLine; Trivia.spaces (4 * level) ]
        | _ -> [ trivia ]

    collectWithAdjacent indentTrivia

let private indentTerminal level = indentPrefix level |> Terminal.mapPrefix

let indentation =
    { new Rewriter<_>() with
        override rewriter.Namespace(level, ns) =
            { base.Namespace(level, ns) with
                  NamespaceKeyword = indentTerminal level ns.NamespaceKeyword }

        override rewriter.NamespaceItem(level, item) =
            base.NamespaceItem(level, item)
            |> NamespaceItem.mapPrefix (indentPrefix level)

        override rewriter.Statement(level, statement) =
            base.Statement(level, statement)
            |> Statement.mapPrefix (indentPrefix level)

        override _.Block(level, mapper, block) =
            { base.Block(level + 1, mapper, block) with
                  CloseBrace = indentTerminal level block.CloseBrace } }

let private ensureNewLine prefix =
    if List.contains NewLine prefix then prefix else NewLine :: prefix

let newLines =
    { new Rewriter<_>() with
        override _.NamespaceItem((), item) =
            base.NamespaceItem((), item)
            |> NamespaceItem.mapPrefix ensureNewLine

        override _.Statement((), statement) =
            let statement = base.Statement((), statement)

            match statement with
            | Else _ -> statement
            | _ -> Statement.mapPrefix ensureNewLine statement

        override _.Block((), mapper, block) =
            let block = base.Block((), mapper, block)

            if List.isEmpty block.Items then
                block
            else
                { block with
                      CloseBrace = Terminal.mapPrefix ensureNewLine block.CloseBrace } }

namespace QsFmt.Formatter.SyntaxTree.Node

type internal 'a ValidNode = { Prefix: string; Kind: 'a }

type internal 'a Node =
    | Missing
    | Valid of 'a ValidNode

module internal Node =
    let map f =
        function
        | Missing -> Missing
        | Valid node -> f node |> Valid

    let withoutPrefix node =
        node
        |> map (fun node -> { node with Prefix = "" })

type internal Terminal = Terminal of string

type internal 'a SequenceItem = { Item: 'a Node; Comma: Terminal Node }

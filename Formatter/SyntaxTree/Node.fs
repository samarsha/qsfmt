module internal QsFmt.Formatter.SyntaxTree.Node

type 'a Node =
    { Kind : 'a option
      TrailingTrivia : string }

type Terminal = Terminal of string

type 'a SequenceItem =
    { Item : 'a Node
      Comma : Terminal Node }

let missingNode = { Kind = None; TrailingTrivia = "" }

let withoutTrivia node = { node with TrailingTrivia = "" }

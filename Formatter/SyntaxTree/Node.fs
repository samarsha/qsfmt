module internal QsFmt.Formatter.SyntaxTree.Node

type 'a Node = { Prefix: string; Kind: 'a option }

type Terminal = Terminal of string

type 'a SequenceItem = { Item: 'a Node; Comma: Terminal Node }

let missingNode = { Prefix = ""; Kind = None }

let withoutPrefix node = { node with Prefix = "" }

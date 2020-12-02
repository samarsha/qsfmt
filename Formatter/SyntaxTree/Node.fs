namespace QsFmt.Formatter.SyntaxTree.Node

type internal Terminal = { Prefix: string; Text: string }

type internal 'a SequenceItem =
    { Item: 'a option
      Comma: Terminal option }

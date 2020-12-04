module internal QsFmt.Formatter.SyntaxTree.Expression

open QsFmt.Formatter.SyntaxTree.Node

type BinaryOperator =
    { Left: Expression
      Operator: Terminal
      Right: Expression }

and Update =
    { Record: Expression
      With: Terminal
      Item: Expression
      Arrow: Terminal
      Value: Expression }

and Expression =
    | MissingExpression of Terminal
    | Literal of Terminal
    | Tuple of Expression Tuple
    | BinaryOperator of BinaryOperator
    | Update of Update

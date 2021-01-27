namespace QsFmt.Formatter.SyntaxTree

type internal BinaryOperator =
    { Left: Expression
      Operator: Terminal
      Right: Expression }

and internal Update =
    { Record: Expression
      With: Terminal
      Item: Expression
      Arrow: Terminal
      Value: Expression }

and internal Expression =
    | Missing of Terminal
    | Literal of Terminal
    | Tuple of Expression Tuple
    | BinaryOperator of BinaryOperator
    | Update of Update
    | Unknown of Terminal

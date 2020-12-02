module internal QsFmt.Formatter.SyntaxTree.Expression

open QsFmt.Formatter.SyntaxTree.Node

type Tuple =
    { OpenParen: Terminal Node
      Items: Expression SequenceItem list
      CloseParen: Terminal Node }

and BinaryOperator =
    { Left: Expression
      Operator: Terminal Node
      Right: Expression }

and Update =
    { Record: Expression
      With: Terminal Node
      Item: Expression
      Arrow: Terminal Node
      Value: Expression }

and Expression =
    | MissingExpression of Terminal Node
    | Literal of Terminal Node
    | Tuple of Tuple
    | BinaryOperator of BinaryOperator
    | Update of Update

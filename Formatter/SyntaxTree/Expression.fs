module internal QsFmt.Formatter.SyntaxTree.Expression

open QsFmt.Formatter.SyntaxTree.Node

type Tuple =
    { OpenParen: Terminal
      Items: Expression SequenceItem list
      CloseParen: Terminal }

and BinaryOperator =
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
    | Tuple of Tuple
    | BinaryOperator of BinaryOperator
    | Update of Update

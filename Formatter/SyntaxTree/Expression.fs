module internal QsFmt.Formatter.SyntaxTree.Expression

open QsFmt.Formatter.SyntaxTree.Node

type Tuple =
    { OpenParen : Terminal Node
      Items : Expression SequenceItem list
      CloseParen : Terminal Node }

and BinaryOperator =
    { Left : Expression Node
      Operator : Terminal Node
      Right : Expression Node }

and Update =
    { Record : Expression Node
      With : Terminal Node
      Item : Expression Node
      Arrow : Terminal Node
      Value : Expression Node }

and Expression =
    | MissingExpression
    | Literal of string
    | Tuple of Tuple
    | BinaryOperator of BinaryOperator
    | Update of Update

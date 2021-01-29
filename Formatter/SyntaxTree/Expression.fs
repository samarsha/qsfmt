namespace QsFmt.Formatter.SyntaxTree

/// An operator applied to two expressions.
type internal BinaryOperator =
    { /// The left-hand side.
      Left: Expression

      /// The operator.
      Operator: Terminal

      /// The right-hand side.
      Right: Expression }

/// A copy-and-update expression.
and internal Update =
    { /// The record to update.
      Record: Expression

      /// The "w/" symbol.
      With: Terminal

      /// The item to update.
      Item: Expression

      /// The "<-" symbol.
      Arrow: Terminal

      /// The value to assign to the item.
      Value: Expression }

/// An expression.
and internal Expression =
    /// An expression that will be provided later.
    | Missing of Terminal

    /// A literal.
    | Literal of Terminal

    /// A tuple expression.
    | Tuple of Expression Tuple

    /// An operator applied to two expressions.
    | BinaryOperator of BinaryOperator

    /// A copy-and-update expression.
    | Update of Update

    /// An unknown expression.
    | Unknown of Terminal

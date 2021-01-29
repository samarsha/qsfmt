namespace QsFmt.Formatter.SyntaxTree

/// A declaration for a new symbol.
type internal SymbolDeclaration =
    { /// The name of the symbol.
      Name: Terminal

      /// The type of the symbol.
      Type: TypeAnnotation option }

/// A binding for one or more new symbols.
type internal SymbolBinding =
    /// A declaration for a new symbol.
    | SymbolDeclaration of SymbolDeclaration

    /// A declaration for a tuple of new symbols.
    | SymbolTuple of SymbolBinding Tuple

/// A "let" statement.
type internal Let =
    { /// The "let" keyword.
      LetKeyword: Terminal

      /// The symbol binding.
      Binding: SymbolBinding

      /// The equals symbol.
      Equals: Terminal

      /// The value of the symbol binding.
      Value: Expression

      /// The semicolon.
      Semicolon: Terminal }

/// A "return" statement.
type internal Return =
    { /// The "return" keyword.
      ReturnKeyword: Terminal

      /// The returned expression.
      Expression: Expression

      /// The semicolon.
      Semicolon: Terminal }

/// An "if" statement.
type internal If =
    { /// The "if" keyword.
      IfKeyword: Terminal

      /// The condition under which to execute the block.
      Condition: Expression

      /// The conditional block.
      Block: Statement Block }

/// An "else" statement.
and internal Else =
    { /// The "else" keyword.
      ElseKeyword: Terminal

      /// The conditional block.
      Block: Statement Block }

/// A statement.
and internal Statement =
    /// A "let" statement.
    | Let of Let

    /// A "return" statement.
    | Return of Return

    /// An "if" statement.
    | If of If

    /// An "else" statement.
    | Else of Else

    /// An unknown statement.
    | Unknown of Terminal

module internal Statement =
    /// <summary>
    /// Maps a statement by applying <paramref name="mapper"/> to its trivia prefix.
    /// </summary>
    let mapPrefix mapper =
        function
        | Let lets ->
            { lets with
                  LetKeyword = lets.LetKeyword |> Terminal.mapPrefix mapper }
            |> Let
        | Return returns ->
            { returns with
                  ReturnKeyword = returns.ReturnKeyword |> Terminal.mapPrefix mapper }
            |> Return
        | If ifs ->
            { ifs with
                  IfKeyword = ifs.IfKeyword |> Terminal.mapPrefix mapper }
            |> If
        | Else elses ->
            { elses with
                  ElseKeyword = elses.ElseKeyword |> Terminal.mapPrefix mapper }
            |> Else
        | Unknown terminal -> Terminal.mapPrefix mapper terminal |> Unknown

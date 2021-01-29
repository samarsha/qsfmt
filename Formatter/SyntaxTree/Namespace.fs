namespace QsFmt.Formatter.SyntaxTree

/// A callable declaration.
type internal CallableDeclaration =
    { /// The declaration keyword (either "function" or "operation").
      CallableKeyword: Terminal

      /// The name of the callable.
      Name: Terminal

      /// The parameters of the callable.
      Parameters: SymbolBinding

      /// The return type of the callable.
      ReturnType: TypeAnnotation

      /// The body of the callable.
      Block: Statement Block }

/// An item in a namespace.
type internal NamespaceItem =
    /// A callable declaration
    | CallableDeclaration of CallableDeclaration

    /// An unknown namespace item.
    | Unknown of Terminal

module internal NamespaceItem =
    /// <summary>
    /// Maps a namespace item by applying <paramref name="mapper"/> to its trivia prefix.
    /// </summary>
    let mapPrefix mapper =
        function
        | CallableDeclaration callable ->
            { callable with
                  CallableKeyword = Terminal.mapPrefix mapper callable.CallableKeyword }
            |> CallableDeclaration
        | Unknown terminal -> Terminal.mapPrefix mapper terminal |> Unknown

/// A namespace
type internal Namespace =
    { /// The "namespace" keyword.
      NamespaceKeyword: Terminal

      /// The name of the namespace.
      Name: Terminal

      /// The body of the namespace.
      Block: NamespaceItem Block }

/// A document representing a Q# file.
type internal Document =
    { /// The namespaces in the document.
      Namespaces: Namespace list

      /// The end-of-file symbol.
      Eof: Terminal }

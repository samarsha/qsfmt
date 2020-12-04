namespace QsFmt.Formatter.SyntaxTree

type internal CallableDeclaration =
    { CallableKeyword: Terminal
      Name: Terminal
      Parameters: SymbolBinding
      ReturnType: TypeAnnotation
      Block: Statement Block }

type internal NamespaceItem = CallableDeclaration of CallableDeclaration

module internal NamespaceItem =
    let mapPrefix mapper (CallableDeclaration callable) =
        { callable with
              CallableKeyword = Terminal.mapPrefix mapper callable.CallableKeyword }
        |> CallableDeclaration

type internal Namespace =
    { NamespaceKeyword: Terminal
      Name: Terminal
      Block: NamespaceItem Block }

type internal Program =
    { Namespaces: Namespace list
      Eof: Terminal }

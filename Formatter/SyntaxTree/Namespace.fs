namespace QsFmt.Formatter.SyntaxTree

type internal CallableDeclaration =
    { CallableKeyword: Terminal
      Name: Terminal
      Parameters: SymbolBinding
      ReturnType: TypeAnnotation
      Block: Statement Block }

type internal NamespaceItem =
    | CallableDeclaration of CallableDeclaration
    | Unknown of Terminal

module internal NamespaceItem =
    let mapPrefix mapper =
        function
        | CallableDeclaration callable ->
            { callable with
                  CallableKeyword = Terminal.mapPrefix mapper callable.CallableKeyword }
            |> CallableDeclaration
        | Unknown text -> Unknown text

type internal Namespace =
    { NamespaceKeyword: Terminal
      Name: Terminal
      Block: NamespaceItem Block }

type internal Program =
    { Namespaces: Namespace list
      Eof: Terminal }

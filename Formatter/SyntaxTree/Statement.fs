namespace QsFmt.Formatter.SyntaxTree

type internal SymbolDeclaration =
    { Name: Terminal
      Type: TypeAnnotation option }

type internal SymbolBinding =
    | SymbolDeclaration of SymbolDeclaration
    | SymbolTuple of SymbolBinding Tuple

type internal Let =
    { LetKeyword: Terminal
      Binding: SymbolBinding
      Equals: Terminal
      Value: Expression
      Semicolon: Terminal }

type internal Return =
    { ReturnKeyword: Terminal
      Expression: Expression
      Semicolon: Terminal }

type internal If =
    { IfKeyword: Terminal
      OpenParen: Terminal
      Condition: Expression
      CloseParen: Terminal
      Block: Statement Block }

and internal Else =
    { ElseKeyword: Terminal
      Block: Statement Block }

and internal Statement =
    | Let of Let
    | Return of Return
    | If of If
    | Else of Else
    | Unknown of Terminal

module internal Statement =
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

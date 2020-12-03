namespace QsFmt.Formatter.SyntaxTree.Statement

open QsFmt.Formatter.SyntaxTree.Expression
open QsFmt.Formatter.SyntaxTree.Node

type internal SymbolBinding =
    | SymbolName of Terminal
    | SymbolTuple of SymbolBinding list

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
      OpenBrace: Terminal
      Statements: Statement list
      CloseBrace: Terminal }

and internal Else =
    { ElseKeyword: Terminal
      OpenBrace: Terminal
      Statements: Statement list
      CloseBrace: Terminal }

and internal Statement =
    | Let of Let
    | Return of Return
    | If of If
    | Else of Else

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

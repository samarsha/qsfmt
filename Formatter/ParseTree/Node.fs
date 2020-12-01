module internal QsFmt.Formatter.ParseTree.Node

open Antlr4.Runtime
open QsFmt.Formatter.SyntaxTree.Node
open QsFmt.Parser
open System.Collections.Immutable

let private hiddenTokensToRight (tokens: IToken ImmutableArray) index =
    seq {
        for i in index + 1 .. tokens.Length - 1 do
            tokens.[i]
    }
    |> Seq.takeWhile (fun token -> token.Channel = QSharpLexer.Hidden)

let private trailingTrivia tokens index =
    hiddenTokensToRight tokens index
    |> Seq.map (fun token -> token.Text)
    |> String.concat ""

let toNode tokens (context: ParserRuleContext) kind =
    { Kind = Some kind
      TrailingTrivia = trailingTrivia tokens context.Stop.TokenIndex }

let toTerminal tokens (terminal: IToken) =
    { Kind = Terminal terminal.Text |> Some
      TrailingTrivia = trailingTrivia tokens terminal.TokenIndex }

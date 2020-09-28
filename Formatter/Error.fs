module internal QsFmt.Formatter.Error

open Antlr4.Runtime
open QsFmt.Parser

type ErrorListListener () =
    let mutable errorTokens = []

    member _.ErrorTokens = errorTokens

    interface IToken IAntlrErrorListener with
        override _.SyntaxError (_, _, token, _, _, _, _) =
            errorTokens <- token :: errorTokens

let private hideToken (token : IToken) : IToken =
    let token' = CommonToken token
    token'.Channel <- QSharpLexer.Hidden
    upcast token'

let hideTokens toHide =
    Seq.map <| fun token ->
        if toHide |> Seq.contains token
        then hideToken token
        else token

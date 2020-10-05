module internal QsFmt.Formatter.Lexer

open QsFmt.Parser

type Lexer (input) =
    inherit QSharpLexer (input)

    override this.PopMode () =
        match this.ModeStack.TryPop () with
        | true, mode -> this.CurrentMode <- mode
        | _ -> ()
        this.CurrentMode

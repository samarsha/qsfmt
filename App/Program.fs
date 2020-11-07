open QsFmt.Formatter

let private example = "\
namespace     Foo {
    function Bar() : Int {
        let x= // Newlines are preserved.
            (7 -   1) // Comments too.
            + 4;
        return  x w/ Foo <- (7, y);
    }
}"

[<EntryPoint>]
let main _ =
    Formatter.format example |> printfn "%s"
    0

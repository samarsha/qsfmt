open QsFmt.Formatter

let private example = "\
namespace     Foo {
    function Bar() : Int {
        let x= // Newlines are preserved.
            (7 -   1) // Comments too.
            + 4;
        return  x;
    }
}"

[<EntryPoint>]
let main _ =
    Formatter.format example |> printfn "%s"
    0

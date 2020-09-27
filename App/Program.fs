open QsFmt.Formatter

let private example = "\
namespace     Foo {
    function Bar() : Int {
        let x=
            (7 -   1) // comment in expression
            + 4; // end let
        return  x;
    } // end function
} // end namespace"

[<EntryPoint>]
let main _ =
    Formatter.format example |> printfn "%s"
    0

open QsFmt.Formatter

let private example = "\
namespace  Foo { // namespace
    function Bar() : Int { // function
        let x=
            (7 -   1)
            + 4; // end let
        return  x; // end return
    } // end function
} // end namespace"

[<EntryPoint>]
let main _ =
    Formatter.format example
    0

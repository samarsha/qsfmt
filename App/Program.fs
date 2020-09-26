open QsFmt.Formatter

let private example = "\
namespace Foo {
    function Bar() : Int {
        let x = 7;
        return x;
    }
}"

[<EntryPoint>]
let main _ =
    Formatter.format example
    0

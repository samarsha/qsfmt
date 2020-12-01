module QsFmt.Formatter.Tests

open Xunit

let private examples =
    [ "\
namespace     Foo {
    function Bar() : Int {
        let x= // Newlines are preserved.
            (7 -   1) // Comments too.
            + 4;
        return  x w/ Foo <- (7, y);
    }
}",
      "\
namespace Foo {
    function Bar() : Int {
        let x = // Newlines are preserved.
            (7 - 1) // Comments too.
            + 4;
        return x w/ Foo <- (7, y);
    }
}" ]

type private ExampleData() as data =
    inherit TheoryData<string, string>()
    do examples |> List.iter data.Add

[<Theory>]
[<ClassData(typeof<ExampleData>)>]
let ``Formatted code matches expected output`` input output =
    Assert.Equal(output, Formatter.format input)

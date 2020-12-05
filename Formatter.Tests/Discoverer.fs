namespace QsFmt.Formatter.Tests

open QsFmt.Formatter
open System
open System.Linq
open System.Reflection
open Xunit

type internal ExampleAttribute() =
    inherit Attribute()

type internal FixedPointAttribute() =
    inherit Attribute()

module Discoverer =
    let private cases (attribute: Type) =
        Assembly.GetCallingAssembly().GetTypes()
        |> Seq.collect (fun typ -> typ.GetProperties())
        |> Seq.filter (fun property ->
            property.GetCustomAttributes attribute
            |> Seq.isEmpty
            |> not)
        |> Seq.map (fun property -> property.GetValue null)
        |> fun values -> values.OfType<'a>()

    type private ExampleData() as data =
        inherit TheoryData<string, string>()

        do
            cases typeof<ExampleAttribute>
            |> Seq.iter data.Add

    type private FixedPointData() as data =
        inherit TheoryData<string>()

        do
            cases typeof<ExampleAttribute>
            |> Seq.map snd<string, _>
            |> Seq.append (cases typeof<FixedPointAttribute>)
            |> Seq.iter data.Add

    [<Theory>]
    [<ClassData(typeof<ExampleData>)>]
    let ``Code is formatted correctly`` input output =
        Assert.Equal(output, Formatter.format input)

    [<Theory>]
    [<ClassData(typeof<FixedPointData>)>]
    let ``Formatted code is unchanged`` input =
        Assert.Equal(input, Formatter.format input)

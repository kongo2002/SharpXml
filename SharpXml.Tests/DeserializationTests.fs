namespace SharpXml.Tests

module DeserializationTests =

    open System
    open System.Diagnostics
    open NUnit.Framework

    open SharpXml
    open SharpXml.Tests.TestHelpers
    open SharpXml.Tests.Types

    let deserialize<'a> input =
        XmlSerializer.DeserializeFromString<'a>(input)

    [<Test>]
    let ``Can deserialize a simple class``() =
        let out = deserialize<TestClass> "<testClass><v1>42</v1><v2>bar</v2></testClass>"
        out.V1 |> should equal 42
        out.V2 |> should equal "bar"

    [<Test>]
    let ``Can deserialize string arrays``() =
        let out = deserialize<TestClass3> "<testClass><v1><item>foo</item><item>bar</item></v1><v2>42</v2></testClass>"
        out.V1.Length |> should equal 2
        out.V1 |> should equal [| "bar"; "foo" |]
        out.V2 |> should equal 42

    [<Test>]
    let ``Can deserialize class arrays``() =
        let out = deserialize<TestClass4> "<testClass4><v1><item><v1>42</v1><v2>foo</v2></item><item><v1>200</v1><v2>bar</v2></item></v1><v2>99</v2></testClass4>"
        out.V1.Length |> should equal 2
        out.V1.[0].V1 |> should equal 200
        out.V1.[1].V2 |> should equal "foo"
        out.V2 |> should equal 99

    [<Test>]
    let ``Can deserialize class lists``() =
        let out = deserialize<ListClass> "<listClass><v1><item><v1>42</v1><v2>foo</v2></item><item><v1>200</v1><v2>bar</v2></item></v1><v2>99</v2></listClass>"
        out.V1.Count |> should equal 2
        out.V1.[0].V1 |> should equal 200
        out.V1.[1].V2 |> should equal "foo"
        out.V2 |> should equal 99

    [<Test>]
    let time01() =
        time (fun () -> deserialize<TestClass> "<testClass><v1>42</v1><v2>bar</v2></testClass>" |> ignore) 1000
        time (fun () -> deserialize<TestClass> "<testClass><v1>42</v1><v2>bar</v2></testClass>" |> ignore) 10000
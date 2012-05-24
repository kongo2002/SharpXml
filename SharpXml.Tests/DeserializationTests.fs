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
    let ``Can deserialize string-keyed dictionaries``() =
        let out = deserialize<DictClass> "<dictClass><v1><item><key>foo</key><value>100</value></item><item><key>bar</key><value>200</value></item></v1><v2>99</v2></dictClass>"
        out.V1.Count |> should equal 2
        out.V1.["foo"] |> should equal 100
        out.V1.["bar"] |> should equal 200
        out.V2 |> should equal 99

    [<Test>]
    let ``Can deserialize enums``() =
        let out = deserialize<EnumClass> "<enumClass><v1>Foo</v1><v2>99</v2></enumClass>"
        out.V1 |> should equal TestEnum.Foo
        out.V2 |> should equal 99

    [<Test>]
    let ``Can deserialize untyped ArrayLists``() =
        let out = deserialize<ArrayListClass> "<arrayListClass><v1>937</v1><v2><item>ham</item><item>eggs</item></v2></arrayListClass>"
        out.V1 |> should equal 937
        out.V2.Count |> should equal 2
        out.V2.[0] |> should equal "eggs"
        out.V2.[1] |> should equal "ham"

    [<Test>]
    let ``Can deserialize generic custom list types``() =
        let out = deserialize<CustomListClass> "<customListClass><v1>100</v1><v2><item>foo</item><item>bar</item></v2></customListClass>"
        out.V1 |> should equal 100
        out.V2 |> shouldBe notNull
        out.V2.Count |> should equal 2
        out.V2.[0] |> should equal "bar"
        out.V2.[1] |> should equal "foo"

    [<Test>]
    let ``Can deserialize generic classes with generic lists``() =
        let out = deserialize<GenericListClass<string>> "<genericListClass><v1>100</v1><v2><item>foo</item><item>bar</item></v2></genericListClass>"
        out.V1 |> should equal 100
        out.V2 |> shouldBe notNull
        out.V2.Count |> should equal 2
        out.V2.[0] |> should equal "bar"
        out.V2.[1] |> should equal "foo"

    [<Test>]
    let ``Can deserialize classes with a static ParseXml function``() =
        let out = deserialize<CustomParserClass> "<customParserClass>200x400</CustomParserClass>"
        out.X |> should equal 200
        out.Y |> should equal 400

    [<Test>]
    let ``Can deserialize classes with a list of ParseXml-like classes``() =
        let out = deserialize<GenericListClass<CustomParserClass>> "<genericListClass><v1>99</v1><v2><item>100x200</item><item>200x400</item></v2></genericListClass>"
        out.V1 |> should equal 99
        out.V2.Count |> should equal 2
        out.V2.[0].Y |> should equal 400
        out.V2.[1].X |> should equal 100

    [<Test>]
    let ``Can deserialize classes with string constructors``() =
        let out = deserialize<StringCtorClass> "<stringCtorClass>300x50</stringCtorClass>"
        out.X |> should equal 300
        out.Y |> should equal 50

    [<Test>]
    let ``Profile simple deserialization``() =
        time (fun () -> deserialize<TestClass> "<testClass><v1>42</v1><v2>bar</v2></testClass>" |> ignore) 1000
        time (fun () -> deserialize<TestClass> "<testClass><v1>42</v1><v2>bar</v2></testClass>" |> ignore) 10000
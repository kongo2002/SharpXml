namespace SharpXml.Tests

module SerializationTests =

    open System
    open System.Collections.Generic
    open System.Diagnostics
    open System.IO
    open System.Runtime.Serialization
    open System.Text.RegularExpressions
    open System.Xml
    open NUnit.Framework

    open SharpXml
    open SharpXml.ValueTypeSerializer
    open SharpXml.Tests.TestHelpers
    open SharpXml.Tests.Types

    let stripNamespaces (input : string) =
        let rgx = Regex(@"\s*xmlns:\w+=""[^""]+""")
        rgx.Replace(input, String.Empty)

    let stripXmlHeader (input : string) =
        let rgx = Regex(@"^<[^>]+>")
        rgx.Replace(input, String.Empty)

    let strip = stripXmlHeader >> stripNamespaces

    let contractSerialize<'a> (element : 'a) =
        use ms = new MemoryStream()
        use xw = XmlWriter.Create(ms)
        let dcs = DataContractSerializer(typeof<'a>)
        dcs.WriteObject(xw, element)
        xw.Flush()
        ms.Seek(0L, SeekOrigin.Begin) |> ignore
        let reader = new StreamReader(ms)
        let output = reader.ReadToEnd() |> strip
        Debug.WriteLine(output)
        output

    let serialize<'a> (element : 'a) =
        XmlSerializer.SerializeToString<'a>(element)

    [<Test>]
    let serializeDateTime01() =
        let curr = DateTime.Now
        let date = curr.Date
        serialize date |> should equal (sprintf "<dateTime>%s</dateTime>" (date.ToString("yyyy-MM-dd")))

    [<Test>]
    let serializeFloat() =
        let value = 2.528
        serialize value |> should equal (sprintf "<double>%.3f</double>" value)

    [<Test>]
    let serializeClass01() =
        let cls = TestClass(800, "foo bar")
        serialize cls |> should equal "<testClass><v1>800</v1><v2>foo bar</v2></testClass>"

    [<Test>]
    let serializeClass02() =
        let cls = TestClass(800, "foo bar")
        serialize cls |> should equal "<testClass><v1>800</v1><v2>foo bar</v2></testClass>"

    [<Test>]
    let serializeDict01() =
        let dict = Dictionary<string, int>()
        dict.Add("foo", 42)
        dict.Add("bar", 200)
        serialize dict |> should equal "<dictionary><key>foo</key><value>42</value><key>bar</key><value>200</value></dictionary>"

    [<Test>]
    let serializeDict02() =
        let dict = Dictionary<int, string>()
        dict.Add(42, "foo")
        dict.Add(200, "bar")
        serialize dict |> should equal "<dictionary><key>42</key><value>foo</value><key>200</key><value>bar</value></dictionary>"

    [<Test>]
    let serializeArray01() =
        let array = [| 35; 200; 42 |]
        serialize array |> should equal "<array><item>35</item><item>200</item><item>42</item></array>"

    [<Test>]
    let serializeNestedClass01() =
        let cls = NestedClass(V1 = "foobar", V2 = TestClass2(V1 = "bar foo", V2 = 200))
        serialize cls |> should equal "<nestedClass><v1>foobar</v1><v2><v1>bar foo</v1><v2>200</v2></v2></nestedClass>"

    [<Test>]
    let serializeNestedClass02() =
        let cls = NestedClass2(V1 = "foobar", V2 = NestedClass2(V1 = "barfoo"))
        serialize cls |> should equal "<nestedClass2><v1>foobar</v1><v2><v1>barfoo</v1></v2></nestedClass2>"

    [<Test>]
    let serializeNestedClass03() =
        let cls = NestedClass2(V1 = "foobar", V2 = NestedClass2(V1 = "barfoo", V2 = NestedClass2(V1 = "ham eggs")))
        serialize cls |> should equal "<nestedClass2><v1>foobar</v1><v2><v1>barfoo</v1><v2><v1>ham eggs</v1></v2></v2></nestedClass2>"

    [<Test>]
    let serializeNestedClass04() =
        let cls = NestedClass2(V1 = "foobar", V2 = NestedClass2(V1 = "barfoo", V2 = NestedClass2()))
        serialize cls |> should equal "<nestedClass2><v1>foobar</v1><v2><v1>barfoo</v1><v2></v2></v2></nestedClass2>"

    [<Test>]
    let compareSerialization01() =
        let cls = ContractClass(V1 = "foo", V2 = 42)
        contractSerialize cls |> should equal "<ContractClass><V1>foo</V1><V2>42</V2></ContractClass>"

    [<Test>]
    let compareSerialization02() =
        let dict = Dictionary<string,int>()
        dict.Add("foo", 42)
        let cls = new ContractClass2(V1 = "bar", V2 = dict)
        contractSerialize cls |> should equal "<ContractClass2><V1>bar</V1><V2><d2p1:KeyValueOfstringint><d2p1:Key>foo</d2p1:Key><d2p1:Value>42</d2p1:Value></d2p1:KeyValueOfstringint></V2></ContractClass2>"

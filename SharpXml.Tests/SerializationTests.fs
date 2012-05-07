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

    [<Test>]
    let serializeDateTime01() =
        let writer = new StringWriter()
        let curr = DateTime.Now
        let date = curr.Date
        Serializer<DateTime>.WriteTag(writer, "date", date)
        writer.ToString() |> should equal (sprintf "<date>%s</date>" (date.ToString("yyyy-MM-dd")))

    [<Test>]
    let serializeFloat() =
        let writer = new StringWriter()
        let value = 2.528
        Serializer.WriteTag(writer, "float", value)
        writer.ToString() |> should equal (sprintf "<float>%.3f</float>" value)

    [<Test>]
    let serializeClass01() =
        let writer = new StringWriter()
        let cls = TestClass(800, "foo bar")
        let func = Serializer<TestClass>.GetWriterFunc()
        func writer cls
        writer.ToString() |> should equal "<v1>800</v1><v2>foo bar</v2>"

    [<Test>]
    let serializeDict01() =
        let writer = new StringWriter()
        let dict = Dictionary<string, int>()
        dict.Add("foo", 42)
        dict.Add("bar", 200)
        let func = Serializer<Dictionary<string,int>>.GetWriterFunc()
        func writer dict
        writer.ToString() |> should equal "<key>foo</key><value>42</value><key>bar</key><value>200</value>"

    [<Test>]
    let serializeDict02() =
        let writer = new StringWriter()
        let dict = Dictionary<string, int>()
        dict.Add("foo", 42)
        dict.Add("bar", 200)
        let func = Serializer<obj>.GetWriterFunc()
        func writer dict
        writer.ToString() |> should equal "<key>foo</key><value>42</value><key>bar</key><value>200</value>"

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

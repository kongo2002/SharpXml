namespace SharpXml.Tests

module DataContractSerializerTests =

    open System
    open System.Collections.Generic
    open System.Diagnostics
    open System.IO
    open System.Runtime.Serialization
    open System.Text
    open System.Text.RegularExpressions
    open System.Xml

    open NUnit.Framework

    open SharpXml.Tests.TestHelpers
    open SharpXml.Tests.Types

    let stripNamespaces input =
        let rgx = Regex(@"\s*xmlns(?::\w+)?=""[^""]+""")
        rgx.Replace(input, String.Empty)

    let stripXmlHeader input =
        let rgx = Regex(@"^<[^>]+>")
        rgx.Replace(input, String.Empty)

    let strip = stripXmlHeader >> stripNamespaces

    let maxQuotas = XmlDictionaryReaderQuotas(MaxStringContentLength = 1024 * 1024)

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

    let contractDeserialize<'a> (input : string) =
        let bytes = Encoding.UTF8.GetBytes(input)
        let t = typeof<'a>
        use reader = XmlDictionaryReader.CreateTextReader(bytes, maxQuotas)
        let serializer = DataContractSerializer(t)
        serializer.ReadObject(reader) :?> 'a

    [<Test>]
    let compareClass() =
        let cls = ContractClass(V1 = "foo", V2 = 42)
        contractSerialize cls |> should equal "<ContractClass><V1>foo</V1><V2>42</V2></ContractClass>"

    [<Test>]
    let compareArray01() =
        let arr = [| "foo"; "bar" |]
        let cls = ContractClass3(V1 = arr, V2 = 99)
        contractSerialize cls |> should equal "<ContractClass3><V1><d2p1:string>foo</d2p1:string><d2p1:string>bar</d2p1:string></V1><V2>99</V2></ContractClass3>"

    [<Test>]
    let compareDictionary() =
        let dict = Dictionary<string,int>()
        dict.Add("foo", 42)
        let cls = new ContractClass2(V1 = "bar", V2 = dict)
        contractSerialize cls |> should equal "<ContractClass2><V1>bar</V1><V2><d2p1:KeyValueOfstringint><d2p1:Key>foo</d2p1:Key><d2p1:Value>42</d2p1:Value></d2p1:KeyValueOfstringint></V2></ContractClass2>"

    [<Test>]
    let compareSpecialChars01() =
        let special = "foo\r\nbar"
        let cls = TestClass(200, special)
        contractSerialize cls |> should equal "<Types.TestClass><v1>200</v1><v2>foo\r\nbar</v2></Types.TestClass>"

    [<Test>]
    let serializeSpecialChars02() =
        let special = "</v2>"
        let cls = TestClass(210, special)
        contractSerialize cls |> should equal "<Types.TestClass><v1>210</v1><v2>&lt;/v2&gt;</v2></Types.TestClass>"

    [<Test>]
    let deserialize01() =
        let cls = contractDeserialize<ContractClass> "<ContractClass><V1>foo</V1><V2>42</V2></ContractClass>"
        cls.V1 |> should equal "foo"
        cls.V2 |> should equal 42

    [<Test>]
    let time01() =
        time (fun () -> contractDeserialize<ContractClass> "<ContractClass><V1>foo</V1><V2>42</V2></ContractClass>" |> ignore) 1000
        time (fun () -> contractDeserialize<ContractClass> "<ContractClass><V1>foo</V1><V2>42</V2></ContractClass>" |> ignore) 10000

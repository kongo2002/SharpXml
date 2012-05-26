namespace SharpXml

open System
open System.Globalization
open System.IO
open System.Text

open SharpXml.Utils

/// XML serializer
type XmlSerializer() =

    static member DeserializeFromString<'T> input : 'T =
        if empty input then Unchecked.defaultof<'T> else
            let reader = Deserializer.getReaderFunc typeof<'T>
            match XmlParser.parseAST input 0 with
            | [ xml ] -> reader xml :?> 'T
            | _ -> invalidArg "the input XML has no root element" "input"

    static member SerializeToString<'T> (element : 'T) =
        let sb = StringBuilder()
        use writer = new StringWriter(sb, CultureInfo.InvariantCulture)
        if XmlConfig.Instance.WriteXmlHeader then writer.Write("<?xml version=\"1.0\" encoding=\"utf-8\"?>")
        Serializer.writeType writer element
        sb.ToString()


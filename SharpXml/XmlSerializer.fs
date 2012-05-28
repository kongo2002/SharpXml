namespace SharpXml

open System
open System.Globalization
open System.IO
open System.Text

open SharpXml.Utils

/// XML serializer
type XmlSerializer() =

    /// UTF-8 encoding without BOM
    static let utf8encoding = UTF8Encoding(false)

    /// Header string for XML output
    static let xmlHeader = "<?xml version=\"1.0\" encoding=\"utf-8\"?>"

    /// Deserialize the input string into the specified type
    static member DeserializeFromString<'T> input =
        if empty input then Unchecked.defaultof<'T> else
            try
                let reader = Deserializer.getReaderFunc typeof<'T>
                match XmlParser.parseAST input 0 with
                | [ xml ] -> reader xml :?> 'T
                | _ -> invalidArg "the input XML has no root element" "input"
            with
            | :? SharpXmlException -> Unchecked.defaultof<'T>

    /// Deserialize the input reader into the specified type
    static member DeserializeFromReader<'T> (reader : TextReader) =
        XmlSerializer.DeserializeFromString<'T>(reader.ReadToEnd())

    /// Deserialize the input stream into the specified type
    static member DeserializeFromStream<'T> (stream : Stream) =
        use reader = new StreamReader(stream, utf8encoding)
        XmlSerializer.DeserializeFromString<'T>(reader.ReadToEnd())

    /// Serialize the given object into a XML string
    static member SerializeToString<'T> (element : 'T) =
        let sb = StringBuilder()
        use writer = new StringWriter(sb, CultureInfo.InvariantCulture)
        if XmlConfig.Instance.WriteXmlHeader then writer.Write(xmlHeader)
        Serializer.writeType writer element
        sb.ToString()

    /// Serialize the given object into XML output using the specified TextWriter
    static member SerializeToWriter<'T> (writer : TextWriter, element : 'T) =
        if XmlConfig.Instance.WriteXmlHeader then writer.Write(xmlHeader)
        Serializer.writeType writer element

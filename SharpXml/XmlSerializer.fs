namespace SharpXml

open System
open System.Globalization
open System.IO
open System.Text

/// XML serializer
type XmlSerializer() =

    static let empty = String.IsNullOrWhiteSpace

    static member DeserializeFromString<'T> input : 'T =
        if empty input then Unchecked.defaultof<'T> else
            match TypeParser.getParser typeof<'T> with
            | Some parser -> unbox parser.Invoke input
            | _ -> Unchecked.defaultof<'T>

    static member SerializeToString<'T> (element : 'T) =
        let sb = StringBuilder()
        use writer = new StringWriter(sb, CultureInfo.InvariantCulture)
        if XmlConfig.Instance.WriteXmlHeader then writer.Write("<?xml version=\"1.0\" encoding=\"utf-8\"?>")
        Serializer<'T>.WriteType(writer, element)
        sb.ToString()


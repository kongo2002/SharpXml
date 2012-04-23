namespace SharpXml

open System

/// XML serializer
type XmlSerializer() =

    let empty = String.IsNullOrWhiteSpace

    let deserializeFromString (targetType : Type) (input : string) =
        if empty input then null else
            match TypeParser.getParser targetType with
            | Some parser -> parser.Invoke input
            | _ -> null

    let serializeToString element =
        String.Empty

    member x.DeserializeFromString<'a> input =
        deserializeFromString typeof<'a> input :?> 'a

    member x.SerializeToString =
        serializeToString


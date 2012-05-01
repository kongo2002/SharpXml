namespace SharpXml

/// Module containing all DateTime related
/// serialization functions
module DateTimeSerializer =

    open System
    open System.Xml
    open SharpXml.Extensions

    let shortDateTimeFormat = "yyyy-MM-dd"
    let defaultFormat = "dd/MM/yyyy HH:mm:ss"
    let defaultFormatWithFraction = "dd/MM/yyyy HH:mm:ss.fff"
    let xsdFormat = "yyyy-MM-ddTHH:mm:ss.fffffffZ"
    let xsdFormat3F = "yyyy-MM-ddTHH:mm:ss.fffZ"
    let xsdFormatSeconds = "yyyy-MM-ddTHH:mm:ssZ"

    /// Convert the given DateTime into XSD format
    let toXsdFormat (date : DateTime) =
        // TODO: replace this with own logic
        XmlConvert.ToString(date.ToUniversal(), XmlDateTimeSerializationMode.Utc)

    /// Convert the given DateTime into the shortest possible XSD format
    let toShortestXsdFormat (date : DateTime) =
        let day = date.TimeOfDay
        if day.Ticks = 0L then date.ToString(shortDateTimeFormat)
        elif day.Milliseconds = 0 then date.ToUniversal().ToString(xsdFormatSeconds)
        else toXsdFormat date

/// Module containing the basic XML serialization logic
module Serializer =

    open System
    open System.Collections.Generic
    open System.Globalization
    open System.IO

    open SharpXml.Attempt
    open SharpXml.Extensions

    /// Writer function delegate
    type WriterFunc = TextWriter -> string -> obj -> unit

    let serializerCache = ref (Dictionary<Type, WriterFunc>())

    let writeTag (writer : TextWriter) (name : string) value writeFunc =
        writer.Write("<{0}>", name)
        writeFunc value
        writer.Write("</{0}>", name)

    let writeString (writer : TextWriter) (name : string) (content : string) =
        writer.Write("<{0}>{1}</{0}>", name, content)

    let nullableWriter writer name value func =
        if value <> null then func writer name value

    let writeStringObject writer name (value : obj) =
        let v : string = unbox value
        writeString writer name v

    let writeObject writer name (value : obj) =
        writeTag writer name value (writer.Write)

    let writeFloat2f writer name (value : obj) =
        let v = sprintf "%.2f" (unbox value)
        writeString writer name v

    let writeDateTime writer name (value : obj) =
        let v = DateTimeSerializer.toShortestXsdFormat (unbox value)
        writeString writer name v

    let writeNullableDateTime writer name (value : obj) =
        nullableWriter writer name value writeDateTime

    let writeDateTimeOffset writer name (value : obj) =
        let v : DateTimeOffset = unbox value
        writeString writer name (v.ToString("o"))

    let writeNullableDateTimeOffset writer name (value : obj) =
        nullableWriter writer name value writeDateTimeOffset

    let writeGuid writer name (value : obj) =
        let v : Guid = unbox value
        writeString writer name (v.ToString("N"))

    let writeNullableGuid writer name (value : obj) =
        nullableWriter writer name value writeGuid

    let writeChar writer name (value : obj) =
        let v : char = unbox value
        writeTag writer name v writer.Write

    let writeChars writer name (value : obj) =
        let v : char[] = unbox value
        writeTag writer name v writer.Write

    let writeByte writer name (value : obj) =
        let v : byte = unbox value
        writeTag writer name v writer.Write

    let writeSByte writer name (value : obj) =
        let v : sbyte = unbox value
        writeTag writer name v writer.Write

    let writeBytes writer name (value : obj) =
        let v = Convert.ToBase64String(unbox value)
        writeString writer name v

    let writeUInt16 writer name (value : obj) =
        let v : uint16 = unbox value
        writeTag writer name v writer.Write

    let writeInt16 writer name (value : obj) =
        let v : int16 = unbox value
        writeTag writer name v writer.Write

    let writeInt32 writer name (value : obj) =
        let v : int = unbox value
        writeTag writer name v writer.Write

    let writeUInt32 writer name (value : obj) =
        let v : uint32 = unbox value
        writeTag writer name v writer.Write

    let writeInt64 writer name (value : obj) =
        let v : int64 = unbox value
        writeTag writer name v writer.Write

    let writeUInt64 writer name (value : obj) =
        let v : uint64 = unbox value
        writeTag writer name v writer.Write

    let writeFloat writer name (value : obj) =
        let v : float = unbox value
        writeTag writer name v writer.Write

    let writeFloat32 writer name (value : obj) =
        let v : float32 = unbox value
        writeTag writer name v writer.Write

    let writeBool writer name (value : obj) =
        let v : bool = unbox value
        // TODO: maybe these should be written in lowercase instead
        writeTag writer name v writer.Write

    let writeDecimal writer name (value : obj) =
        let v : decimal = unbox value
        writeString writer name (v.ToString(CultureInfo.InvariantCulture))

    let writeEnum writer name (value : obj) =
        writeObject writer name value

    let writeEnumNames writer name (value : obj) =
        let v : int = unbox value
        writeTag writer name v (writer.Write)

    let writeType writer name (value : obj) =
        let v : Type = unbox value
        writeString writer name v.AssemblyQualifiedName

    let writeException writer name (value : obj) =
        let v : Exception = unbox value
        writeString writer name v.Message

    /// Get the appropriate writer function for the
    /// specified value type
    let getValueTypeWriter (t : Type) =
        if t = typeof<Nullable<DateTime>> then
            Some writeNullableDateTime
        elif t = typeof<Guid> then
            Some writeGuid
        elif t = typeof<Nullable<Guid>> then
            Some writeNullableGuid
        elif t = typeof<DateTimeOffset> then
            Some writeDateTimeOffset
        elif t = typeof<Nullable<DateTimeOffset>> then
            Some writeNullableDateTimeOffset
        elif t.IsEnum || t.UnderlyingSystemType.IsEnum then
            if hasAttribute t "FlagsAttribute" then Some writeEnumNames else Some writeEnum
        else
            match Type.GetTypeCode(t.NullableUnderlying()) with
            | TypeCode.Boolean -> Some writeBool
            | TypeCode.Byte -> Some writeByte
            | TypeCode.Char -> Some writeChar
            | TypeCode.DateTime -> Some writeDateTime
            | TypeCode.Decimal -> Some writeDecimal
            | TypeCode.Double -> Some writeFloat32
            | TypeCode.Int16 -> Some writeInt16
            | TypeCode.Int32 -> Some writeInt32
            | TypeCode.Int64 -> Some writeInt64
            | TypeCode.SByte -> Some writeSByte
            | TypeCode.Single -> Some writeFloat
            | TypeCode.UInt16 -> Some writeUInt16
            | TypeCode.UInt32 -> Some writeUInt32
            | TypeCode.UInt64 -> Some writeUInt64
            | _ -> None

    /// Try to determine one of a special serialization
    /// function, i.e. Exception, Uri
    let getSpecialWriters (t : Type) =
        if t = typeof<Uri> then Some writeStringObject
        elif t = typeof<Exception> then Some writeException
        elif t = typeof<Type> then Some writeType
        else None

    /// Determine the associated serialization writer
    /// function for the specified type
    let determineWriter (t : Type) =
        let none = fun () -> None
        let func f = fun () -> Some f
        let iff p f = if p t then (fun () -> f) else none
        let writer = attempt {
            let! strWriter = iff ((=) typeof<string>) (Some writeStringObject)
            let! valueWriter = iff (fun x -> x.IsValueType) (getValueTypeWriter t)
            let! arrayWriter =
                if t.IsArray then
                    if t = typeof<byte[]> then (func writeBytes)
                    elif t = typeof<char[]> then (func writeChars)
                    // TODO: often used arrays like string[] and int[]
                    else none
                else none
            arrayWriter }
        writer

    /// Get the writer function to serialize the
    /// specified type
    let getWriterFunc (t : Type) =
        match (!serializerCache).TryGetValue t with
        | true, serializer -> serializer
        | _ ->
            match determineWriter t with
            | Some s -> Atom.updateAtomDict serializerCache t s
            | _ -> invalidOp "No serializer available for type %s" t.FullName
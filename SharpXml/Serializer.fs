namespace SharpXml

/// Writer function delegate
type WriterFunc = System.IO.TextWriter -> obj -> unit

/// Module containing the serialization logic
/// for value types
module ValueTypeSerializer =

    open System
    open System.Globalization
    open System.IO
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

    let writeString (writer : TextWriter) (content : string) =
        writer.Write(content)

    let nullableWriter writer value func =
        if value <> null then func writer value

    let writeStringObject writer (value : obj) =
        let v : string = unbox value
        writeString writer v

    let writeObject (writer : TextWriter) (value : obj) =
        writer.Write(value)

    let writeFloat2f (writer : TextWriter) (value : obj) =
        let v = sprintf "%.2f" (unbox value)
        writeString writer v

    let writeDateTime writer (value : obj) =
        let v = toShortestXsdFormat (unbox value)
        writeString writer v

    let writeNullableDateTime writer (value : obj) =
        nullableWriter writer value writeDateTime

    let writeDateTimeOffset writer (value : obj) =
        let v : DateTimeOffset = unbox value
        writeString writer (v.ToString("o"))

    let writeNullableDateTimeOffset writer (value : obj) =
        nullableWriter writer value writeDateTimeOffset

    let writeGuid writer (value : obj) =
        let v : Guid = unbox value
        writeString writer (v.ToString("N"))

    let writeNullableGuid writer (value : obj) =
        nullableWriter writer value writeGuid

    let writeChar (writer : TextWriter) (value : obj) =
        let v : char = unbox value
        writer.Write(v)

    let writeChars (writer : TextWriter) (value : obj) =
        let v : char[] = unbox value
        writer.Write(v)

    let writeByte (writer : TextWriter) (value : obj) =
        let v : byte = unbox value
        writer.Write(v)

    let writeSByte (writer : TextWriter) (value : obj) =
        let v : sbyte = unbox value
        writer.Write(v)

    let writeBytes (writer : TextWriter) (value : obj) =
        let v = Convert.ToBase64String(unbox value)
        writer.Write(v)

    let writeUInt16 (writer : TextWriter) (value : obj) =
        let v : uint16 = unbox value
        writer.Write(v)

    let writeInt16 (writer : TextWriter) (value : obj) =
        let v : int16 = unbox value
        writer.Write(v)

    let writeInt32 (writer : TextWriter) (value : obj) =
        let v : int = unbox value
        writer.Write(v)

    let writeUInt32 (writer : TextWriter) (value : obj) =
        let v : uint32 = unbox value
        writer.Write(v)

    let writeInt64 (writer : TextWriter) (value : obj) =
        let v : int64 = unbox value
        writer.Write(v)

    let writeUInt64 (writer : TextWriter) (value : obj) =
        let v : uint64 = unbox value
        writer.Write(v)

    let writeFloat (writer : TextWriter) (value : obj) =
        let v : float = unbox value
        writer.Write(v.ToString(CultureInfo.InvariantCulture))

    let writeFloat32 (writer : TextWriter) (value : obj) =
        let v : float32 = unbox value
        writer.Write(v.ToString(CultureInfo.InvariantCulture))

    let writeBool (writer : TextWriter) (value : obj) =
        let v : bool = unbox value
        match v with
        | true -> writer.Write("true")
        | false -> writer.Write("false")

    let writeDecimal writer (value : obj) =
        let v : decimal = unbox value
        writeString writer (v.ToString(CultureInfo.InvariantCulture))

    let writeEnum writer (value : obj) =
        writeObject writer value

    let writeEnumNames (writer : TextWriter) (value : obj) =
        let v : int = unbox value
        writer.Write(v)

    let writeType writer (value : obj) =
        let v : Type = unbox value
        writeString writer v.AssemblyQualifiedName

    let writeException writer (value : obj) =
        let v : Exception = unbox value
        writeString writer v.Message

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
            if t.HasAttribute("FlagsAttribute") then Some writeEnumNames else Some writeEnum
        else
            match Type.GetTypeCode(t.NullableUnderlying()) with
            | TypeCode.Boolean -> Some writeBool
            | TypeCode.Byte -> Some writeByte
            | TypeCode.Char -> Some writeChar
            | TypeCode.DateTime -> Some writeDateTime
            | TypeCode.Decimal -> Some writeDecimal
            | TypeCode.Double -> Some writeFloat
            | TypeCode.Int16 -> Some writeInt16
            | TypeCode.Int32 -> Some writeInt32
            | TypeCode.Int64 -> Some writeInt64
            | TypeCode.SByte -> Some writeSByte
            | TypeCode.Single -> Some writeFloat32
            | TypeCode.UInt16 -> Some writeUInt16
            | TypeCode.UInt32 -> Some writeUInt32
            | TypeCode.UInt64 -> Some writeUInt64
            | _ -> None

open System
open System.Collections
open System.Collections.Generic
open System.IO
open System.Reflection

open SharpXml.Attempt
open SharpXml.Extensions

/// Record type containing the serialization information
/// for a specific property member
type internal PropertyWriterInfo<'T> = {
    Info : PropertyInfo
    OriginalName : string
    ClsName : string
    GetFunc : Reflection.GetterFunc<'T>
    WriteFunc : WriterFunc option
    Default : obj }

/// Serialization logic
type Serializer<'T> private() =

    static let propertyCache = ref (Dictionary<Type, PropertyWriterInfo<'T>[]>())
    static let serializerCache = ref (Dictionary<Type, WriterFunc>())

    /// General purpose XML tags writer function
    static let writeTag (w : TextWriter) (name : string) (value : obj) writeFunc =
        w.Write("<{0}>", name)
        writeFunc w (box value)
        w.Write("</{0}>", name)

    /// Try to determine one of a special serialization
    /// function, i.e. Exception, Uri
    static let getSpecialWriters (t : Type) =
        if t = typeof<Uri> then Some ValueTypeSerializer.writeStringObject
        elif t = typeof<Exception> then Some ValueTypeSerializer.writeException
        elif t = typeof<Type> then Some ValueTypeSerializer.writeType
        else None

    static let writeEmpty (writer : TextWriter) _ = ()

    static let writeAbstractProperties (writer : TextWriter) (value : obj) =
        ()

    /// Build a PropertyWriterInfo object based on the
    /// specified PropertyInfo
    static let rec buildPropertyWriterInfo (propInfo : PropertyInfo) =
        { Info = propInfo
          OriginalName = propInfo.Name
          ClsName = propInfo.Name.ToCamelCase()
          GetFunc = Reflection.getGetter propInfo
          WriteFunc = determineWriter propInfo.PropertyType
          Default = Reflection.getDefaultValue propInfo.PropertyType }

    /// Writer function for IEnumerable
    and writeEnumerable (writer : TextWriter) (value : obj) =
        let collection : IEnumerable = unbox value
        let write func elem =
            let f = match func with | None -> determineWriter (elem.GetType()) | _ -> func
            f.Value writer elem
            f
        collection
        |> Seq.cast
        |> Seq.fold write None
        |> ignore

    /// Try to determine a enumerable serialization function
    and getEnumerableWriter (t : Type) =
        if t.HasInterface typeof<IEnumerable> &&
            t.IsAssignableFrom typeof<IEnumerable> then
            Some writeEnumerable
        else None

    /// Get the PropertyWriterInfo array for the given type
    and getProperties (t : Type) =
        match (!propertyCache).TryGetValue t with
        | true, props -> props
        | _ ->
            let buildWriter = buildPropertyWriterInfo
            let props =
                Reflection.getSerializableProperties t
                |> Seq.filter (fun p -> p.GetIndexParameters().Length = 0)
                |> Seq.map buildWriter
                |> Array.ofSeq
            Atom.updateAtomDict propertyCache t props

    /// Writer for classes and other reference types
    and writeClass (writer : TextWriter) (value : obj) =
        let t = value.GetType()
        getProperties t
        |> Seq.iter (fun p ->
            match p.WriteFunc with
            | Some write ->
                let v = p.GetFunc.Invoke(value :?> 'T)
                if v <> null then
                    writeTag writer p.ClsName v write
            | _ -> ())

    /// Try to determine a class or interface serialization function
    and getClassWriter (t : Type) =
        if t.IsClass || t.IsInterface then
            if t.IsAbstract && not t.IsInterface then
                Some writeAbstractProperties
            else
                Some writeClass
        else None

    /// Determine the associated serialization writer
    /// function for the specified type
    and determineWriter (t : Type) =
        let none = fun () -> None
        let func f = fun () -> Some f
        let iff p f = if p t then (fun () -> f) else none
        let writer = attempt {
            let! strWriter = iff ((=) typeof<string>) (Some ValueTypeSerializer.writeStringObject)
            let! valueWriter = iff (fun x -> x.IsValueType) (ValueTypeSerializer.getValueTypeWriter t)
            let! arrayWriter =
                if t.IsArray then
                    if t = typeof<byte[]> then (func ValueTypeSerializer.writeBytes)
                    elif t = typeof<char[]> then (func ValueTypeSerializer.writeChars)
                    // TODO: often used arrays like string[] and int[]
                    else none
                else none
            let! genericWriter =
                if t.IsGenericType then none
                else none
            let! enumerableWriter = fun () -> getEnumerableWriter t
            let! classWriter = fun () -> getClassWriter t
            classWriter }
        writer

    /// Get the writer function to serialize the
    /// specified type
    static let getWriterFunc (t : Type) =
        match (!serializerCache).TryGetValue t with
        | true, serializer -> serializer
        | _ ->
            match determineWriter t with
            | Some s -> Atom.updateAtomDict serializerCache t s
            | _ -> invalidOp <| sprintf "No serializer available for type '%s'" t.FullName

    static member WriteTag(writer : TextWriter, name : string, element : 'T) =
        writeTag writer name element (getWriterFunc typeof<'T>)

    static member GetWriterFunc() =
        getWriterFunc typeof<'T>

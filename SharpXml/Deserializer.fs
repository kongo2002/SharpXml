namespace SharpXml

/// Reader function delegate
type ReaderFunc = XmlParser.XmlElem -> obj

/// Record type containing the deserialization information
/// for a specific property member
type PropertyReaderInfo = {
    Info : System.Reflection.PropertyInfo
    Reader : ReaderFunc
    Setter : Reflection.SetterFunc }

/// Record type containing the deserialization information
/// of a specific type and all its members that have to deserialized
type TypeBuilderInfo = {
    Type : System.Type
    // TODO: I would love to use a case-insensitive FSharpMap instead
    Props : System.Collections.Generic.Dictionary<string, PropertyReaderInfo>
    Ctor : Reflection.EmptyConstructor }

module ValueTypeDeserializer =

    open System

    open SharpXml.XmlParser

    let inline buildValueReader reader = fun xml ->
        match xml with
        | ContentElem(_, str) -> reader str
        | _ -> null

    let getEnumReader (t : Type) = fun () ->
        if t.IsEnum then
            fun i -> Enum.Parse(t, i)
            |> buildValueReader
            |> Some
        else None

    let getValueReader (t : Type) = fun () ->
        let reader =
            match Type.GetTypeCode(t) with
            | TypeCode.Boolean -> Boolean.Parse >> box |> Some
            | TypeCode.Byte -> Byte.Parse >> box |> Some
            | TypeCode.Int16 -> Int16.Parse >> box |> Some
            | TypeCode.Int32 -> Int32.Parse >> box |> Some
            | TypeCode.Int64 -> Int64.Parse >> box |> Some
            | TypeCode.Char -> Char.Parse >> box |> Some
            | TypeCode.DateTime -> DateTime.Parse >> box |> Some
            | TypeCode.Decimal -> Decimal.Parse >> box |> Some
            | TypeCode.Double -> Double.Parse >> box |> Some
            | TypeCode.SByte -> SByte.Parse >> box |> Some
            | TypeCode.Single -> Single.Parse >> box |> Some
            | TypeCode.UInt16 -> UInt16.Parse >> box |> Some
            | TypeCode.UInt32 -> UInt32.Parse >> box |> Some
            | TypeCode.UInt64 -> UInt64.Parse >> box |> Some
            | TypeCode.String -> box |> Some
            | _ -> None
        match reader with
        | Some r -> Some(buildValueReader r)
        | _ -> None

/// Deserialization logic
module Deserializer =

    open System
    open System.Collections.Generic
    open System.Reflection

    open SharpXml.Attempt
    open SharpXml.Extensions
    open SharpXml.XmlParser

    /// Name of the static parsing method
    let parseMethodName = "ParseXml"

    /// BindingFlags to find the static parse method
    let parseMethodFlags = BindingFlags.Public ||| BindingFlags.Static

    /// TypeBuilder dictionary
    let propertyCache = ref (Dictionary<Type, TypeBuilderInfo>())

    /// Reader function cache
    let readerCache = ref (Dictionary<Type, ReaderFunc>())

    /// Try to find a constructor of the specified type
    /// with a single string parameter
    let findStringConstructor (t : Type) =
        t.GetConstructors()
        |> Array.tryFind (fun ctor ->
            let ps = ctor.GetParameters()
            ps.Length = 1 && ps.[0].ParameterType = typeof<string>)

    /// Try to get a reader based on a string value constructor
    let getStringTypeConstructor (t : Type) = fun () ->
        match findStringConstructor t with
        | Some ctor ->
            fun (v : string) -> ctor.Invoke([| v |])
            |> ValueTypeDeserializer.buildValueReader
            |> Some
        | _ -> None

    /// Try to find the static 'ParseXml' method on the specified type
    let findStaticParseMethod (t : Type) =
        t.GetMethod(parseMethodName, parseMethodFlags, null, [| typeof<string> |], null)
        |> Utils.toOption

    /// Try to get a reader based on the type's static 'ParseXml' method
    let getStaticParseMethod (t : Type) = fun () ->
        match findStaticParseMethod t with
        | Some parse ->
            // TODO: maybe use Delegate.CreateDelegate()
            fun (v : string) -> parse.Invoke(null, [| v |])
            |> ValueTypeDeserializer.buildValueReader
            |> Some
        | _ -> None

    /// Parse one element for a deserialised list structure
    let parseListElement<'a> (reader : ReaderFunc) element =
        match reader(element) with
        | null -> None
        | x -> Some(x :?> 'a)

    /// Build the PropertyReaderInfo record based on the given PropertyInfo
    let rec buildReaderInfo (p : PropertyInfo) =
        { Info = p; Reader = getReaderFunc p.PropertyType; Setter = Reflection.getObjSetter p }

    /// Build the TypeBuilderInfo record for the given Type
    and buildTypeBuilderInfo (t : Type) =
        let map =
            Reflection.getSerializableProperties t
            |> Seq.map (fun p -> p.Name, buildReaderInfo p)
            |> dict
        { Type = t
          Props = Dictionary(map, StringComparer.OrdinalIgnoreCase)
          Ctor = Reflection.getEmptyConstructor t }

    /// Determine the TypeBuilderInfo for the given Type
    and getTypeBuilderInfo (t : Type) =
        match (!propertyCache).TryGetValue t with
        | true, builder -> builder
        | _ ->
            let builder = buildTypeBuilderInfo t
            Atom.updateAtomDict propertyCache t builder

    /// Reader function for immutable F# lists
    and listReader<'a> (reader : ReaderFunc) xml =
        match xml with
        | GroupElem(_, elems) ->
            elems |> List.choose (parseListElement<'a> reader)
        | _ -> []

    /// Reader function for CLR list (System.Collections.Generic.List<T>)
    and clrListReader<'a> (reader : ReaderFunc) xml =
        let list = List<'a>()
        match xml with
        | GroupElem(_, elems) ->
            elems
            |> List.choose (parseListElement<'a> reader)
            |> list.AddRange
        | _ -> ()
        list

    and getTypedListReader (t : Type) =
        // TODO: this does not look sane at all
        let reader = Type.GetType("SharpXml.Deserializer").GetMethod("clrListReader")
        let mtd = reader.MakeGenericMethod([| t |])
        let elemReader = getReaderFunc t
        fun (xml : XmlElem) -> mtd.Invoke(null, [| elemReader; xml |])

    /// Reader function for arrays
    and arrayReader<'a> (reader : ReaderFunc) xml =
        match xml with
        | GroupElem(_, elems) ->
            elems
            |> List.choose (parseListElement<'a> reader)
            |> Array.ofList
        | _ -> [| |]

    and getTypedArrayReader (t : Type) =
        // TODO: this does not look sane at all
        let reader = Type.GetType("SharpXml.Deserializer").GetMethod("arrayReader")
        let mtd = reader.MakeGenericMethod([| t |])
        let elemReader = getReaderFunc t
        fun (xml : XmlElem) -> mtd.Invoke(null, [| elemReader; xml |])

    /// Specialized reader function for string arrays
    and stringArrayReader xml =
        let stringReader = box |> ValueTypeDeserializer.buildValueReader
        listReader<string> stringReader xml |> List.toArray |> box

    /// Specialized reader function for byte arrays
    and byteArrayReader xml =
        let byteReader = Byte.Parse >> box |> ValueTypeDeserializer.buildValueReader
        listReader<byte> byteReader xml |> List.toArray |> box

    /// Specialized reader function for integer arrays
    and intArrayReader xml =
        let intReader = Int32.Parse >> box |> ValueTypeDeserializer.buildValueReader
        listReader<int> intReader xml |> List.toArray |> box

    /// Try to determine a reader function for array types
    and getArrayReader (t : Type) = fun () ->
        if not t.IsArray then None else
            if t = typeof<string[]> then Some stringArrayReader
            elif t = typeof<int[]> then Some intArrayReader
            elif t = typeof<byte[]> then Some byteArrayReader
            else
                let elem = t.GetElementType()
                Some <| getTypedArrayReader elem

    /// Try to determine a reader function for list types
    and getListReader (t : Type) = fun () ->
        if t.IsGenericType then
            let listInterface = TypeHelper.getTypeWithGenericType t typedefof<IList<_>>
            match listInterface with
            | Some listType ->
                let elem = listType.GetGenericArguments().[0]
                Some <| getTypedListReader elem
            | _ -> None
        else None

    /// Class reader function
    and readClass (builder : TypeBuilderInfo) xml =
        let instance = builder.Ctor.Invoke()
        let rec inner inst elems =
            match elems with
            | GroupElem(name, _)  as h :: t ->
                match builder.Props.TryGetValue name with
                | true, prop ->
                    let reader = prop.Reader
                    prop.Setter.Invoke(inst, reader(h))
                    inner inst t
                | _ -> inner inst t
            | ContentElem(name, _) as h :: t ->
                match builder.Props.TryGetValue name with
                | true, prop ->
                    let reader = prop.Reader
                    prop.Setter.Invoke(inst, reader(h))
                    inner inst t
                | _ -> inner inst t
            | SingleElem _ :: t ->
                // TODO: maybe we want to set the default value in here
                inner inst t
            | [] -> ()
        match xml with
        | GroupElem(_, subElements) -> inner instance subElements
        | _ -> ()
        instance

    /// Try to determine a matching class reader function
    and getClassReader (t : Type) = fun () ->
        if t.IsClass && not t.IsAbstract then
            getTypeBuilderInfo t
            |> readClass
            |> Some
        else None

    /// Determine the ReaderFunc delegate for the given Type
    and determineReader (objType : Type) =
        let t = objType.NullableUnderlying()
        let reader = attempt {
            let! enumReader = ValueTypeDeserializer.getEnumReader t
            let! valueReader = ValueTypeDeserializer.getValueReader t
            let! arrayReader = getArrayReader t
            let! listReader = getListReader t
            let! staticReader = getStaticParseMethod t
            let! stringCtor = getStringTypeConstructor t
            let! classReader = getClassReader t
            classReader }
        reader

    /// Get the ReaderFunc for the specified type.
    /// The function is either obtained from the cache or built on request
    and getReaderFunc (t : Type) =
        match (!readerCache).TryGetValue t with
        | true, reader -> reader
        | _ ->
            match determineReader t with
            | Some reader -> Atom.updateAtomDict readerCache t reader
            | _ ->
                let err = sprintf "could not determine deserialization logic for type '%s'" t.FullName
                invalidOp err
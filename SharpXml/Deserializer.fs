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

    let inline extractString xml =
        match xml with
        | ContentElem(_, str) -> str
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

/// Dictionary related deserialization logic
module DictionaryDeserializer =

    open System.Collections.Generic
    open System.Collections.Specialized

    open SharpXml.XmlParser

    /// Dictionary reader function
    let dictReader<'a, 'b when 'a : equality> (keyReader : ReaderFunc) (valueReader : ReaderFunc) xml =
        let readDictElem = function
            // TODO: key and value tags are reversed
            | GroupElem(_, [ v; k ]) ->
                let key = keyReader(k) :?> 'a
                let value = valueReader(v) :?> 'b
                (key, value) |> Some
            | _ -> None
        let dictionary = Dictionary<'a, 'b>()
        match xml with
        | GroupElem(_, elements) ->
            elements
            |> List.choose readDictElem
            |> List.iter (fun (k, v) -> dictionary.[k] <- v)
        | _ -> ()
        dictionary

    let nameValueCollectionReader (ctor : unit -> #NameValueCollection) xml =
        let collection = ctor()
        let processElement = function
            | GroupElem(_, [ v; k ]) ->
                let key = ValueTypeDeserializer.extractString k
                let value = ValueTypeDeserializer.extractString v
                collection.[key] <- value
            | _ -> ()
        match xml with
        | GroupElem(_, elements) -> List.iter processElement elements
        | _ -> ()
        box collection

/// List related deserialization logic
module ListDeserializer =

    open System
    open System.Collections
    open System.Collections.Generic
    open System.Collections.Specialized

    open SharpXml.XmlParser

    /// Parse one element for a deserialised list structure
    let inline parseListElement<'a> (reader : ReaderFunc) element =
        match reader(element) with
        | null -> None
        | x -> Some(x :?> 'a)

    /// Generic collection processing function
    let collectionProcessor<'a> action (reader : ReaderFunc) xml =
        match xml with
        | GroupElem(_, elems) ->
            elems
            |> List.choose (parseListElement<'a> reader)
            |> List.iter action
        | _ -> ()

    /// Reader function for immutable F# lists
    let listReader<'a> (reader : ReaderFunc) xml =
        match xml with
        | GroupElem(_, elems) ->
            elems |> List.choose (parseListElement<'a> reader)
        | _ -> []

    /// Reader function for CLR list (System.Collections.Generic.List<T>)
    let clrListReader<'a> (reader : ReaderFunc) xml =
        let list = List<'a>()
        match xml with
        | GroupElem(_, elems) ->
            elems
            |> List.choose (parseListElement<'a> reader)
            |> list.AddRange
        | _ -> ()
        list

    /// Reader function for arrays
    let arrayReader<'a> (reader : ReaderFunc) xml =
        match xml with
        | GroupElem(_, elems) ->
            elems
            |> List.choose (parseListElement<'a> reader)
            |> Array.ofList
        | _ -> [| |]

    /// Reader function for untyped collections
    let collectionReader (ctor : Reflection.EmptyConstructor) xml =
        let list = ctor.Invoke() :?> IList
        match xml with
        | GroupElem(_, elems) ->
            elems
            |> List.choose (parseListElement (box |> ValueTypeDeserializer.buildValueReader))
            |> List.iter (list.Add >> ignore)
        | _ -> ()
        list

    /// Reader function for hash sets
    let hashSetReader<'a> (reader : ReaderFunc) xml =
        let set = HashSet<'a>()
        collectionProcessor (set.Add >> ignore) reader xml
        set

    /// Reader function for generic collections
    let genericCollectionReader<'a> (reader : ReaderFunc) (ctor : Reflection.EmptyConstructor) xml =
        let collection = ctor.Invoke() :?> ICollection<'a>
        collectionProcessor collection.Add reader xml
        collection

    let genericROReader<'a> (reader : ReaderFunc) (ctor : System.Reflection.ConstructorInfo) xml =
        let list = clrListReader<'a> reader xml
        ctor.Invoke([| list |])

    /// Reader function for queues
    let queueReader<'a> (reader : ReaderFunc) xml =
        let queue = Queue<'a>()
        collectionProcessor queue.Enqueue reader xml
        queue

    /// Reader function for stacks
    let stackReader<'a> (reader : ReaderFunc) xml =
        let stack = Stack<'a>()
        collectionProcessor stack.Push reader xml
        stack

    /// Reader function for generic linked lists
    let linkedListReader<'a> (reader : ReaderFunc) xml =
        let list = LinkedList<'a>()
        let addTo (item : 'a) = list.AddLast(item) |> ignore
        collectionProcessor addTo reader xml
        list

    /// Specialized reader function for string arrays
    let stringArrayReader xml =
        let stringReader = box |> ValueTypeDeserializer.buildValueReader
        listReader<string> stringReader xml |> List.toArray |> box

    /// Specialized reader function for byte arrays
    let byteArrayReader xml =
        let byteReader = Byte.Parse >> box |> ValueTypeDeserializer.buildValueReader
        listReader<byte> byteReader xml |> List.toArray |> box

    /// Specialized reader function for integer arrays
    let intArrayReader xml =
        let intReader = Int32.Parse >> box |> ValueTypeDeserializer.buildValueReader
        listReader<int> intReader xml |> List.toArray |> box

/// Deserialization logic
module Deserializer =

    open System
    open System.Collections
    open System.Collections.Generic
    open System.Collections.ObjectModel
    open System.Collections.Specialized
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

    /// Active pattern wrapper for generic type detection
    let (|GenericTypeOf|_|) (genericType : Type) (t : Type) =
        match TypeHelper.getTypeWithGenericType t genericType with
        | Some genType -> Some <| genType.GetGenericArguments().[0]
        | _ -> None

    let (|GenericTypeIn|_|) (genericTypes : Type seq) (t : Type) =
        if TypeHelper.hasGenericTypeDefinitions t genericTypes then
            Some <| t.GetGenericArguments().[0]
        else
            None

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

    let getGenericListFunction name t =
        // TODO: I don't like this string-based reflection at all
        let reader = Type.GetType("SharpXml.ListDeserializer").GetMethod(name)
        reader.MakeGenericMethod([| t |])

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

    /// Get a reader function for NameValueCollection types
    let getNameValueCollectionReader (t : Type) =
        let ctor =
            if t = typeof<NameValueCollection> then
                fun() -> NameValueCollection()
            else
                let func = Reflection.getConstructorMethod t
                fun() -> func.Invoke() :?> NameValueCollection
        Some <| DictionaryDeserializer.nameValueCollectionReader ctor

    /// Build the PropertyReaderInfo record based on the given PropertyInfo
    let rec buildReaderInfo (p : PropertyInfo) = {
        Info = p;
        Reader = getReaderFunc p.PropertyType;
        Setter = Reflection.getObjSetter p }

    /// Build the TypeBuilderInfo record for the given Type
    and buildTypeBuilderInfo (t : Type) =
        let map =
            Reflection.getSerializableProperties t
            |> Seq.map (fun p -> p.Name, buildReaderInfo p)
            |> dict
        { Type = t
          Props = Dictionary(map, StringComparer.OrdinalIgnoreCase)
          Ctor = Reflection.getConstructorMethod t }

    /// Determine the TypeBuilderInfo for the given Type
    and getTypeBuilderInfo (t : Type) =
        match (!propertyCache).TryGetValue t with
        | true, builder -> builder
        | _ ->
            let builder = buildTypeBuilderInfo t
            Atom.updateAtomDict propertyCache t builder

    and buildGenericFunction name t =
        let mtd = getGenericListFunction name t
        let elemReader = getReaderFunc t
        fun (xml : XmlElem) -> mtd.Invoke(null, [| elemReader; xml |])

    /// Get a reader function for generic lists
    and getTypedListReader =
        buildGenericFunction "clrListReader"

    /// Get a reader function for arrays
    and getTypedArrayReader =
        buildGenericFunction "arrayReader"

    /// Get a reader function for hash sets
    and getHashSetReader =
        buildGenericFunction "hashSetReader"

    /// Get a reader function for queues
    and getQueueReader =
        buildGenericFunction "queueReader"

    /// Get a reader function for stacks
    and getStackReader =
        buildGenericFunction "stackReader"

    /// Get a reader function for generic linked lists
    and getLinkedListReader =
        buildGenericFunction "linkedListReader"

    /// Get a reader function for generic collections
    and getGenericCollectionReader (listType : Type) (t : Type) =
        let mtd = getGenericListFunction "genericCollectionReader" t
        let elemReader = getReaderFunc t
        let ctor = Reflection.getConstructorMethod listType
        fun (xml : XmlElem) -> mtd.Invoke(null, [| elemReader; ctor; xml |])

    /// Get a reader function for generic readonly collections
    and getGenericROReader ctor (listType : Type) (t : Type) =
        let mtd = getGenericListFunction "genericROReader" t
        let elemReader = getReaderFunc t
        fun (xml : XmlElem) -> mtd.Invoke(null, [| elemReader; ctor; xml |])

    /// Try to determine a reader function for array types
    and getArrayReader (t : Type) = fun () ->
        if not t.IsArray then None else
            if t = typeof<string[]> then Some ListDeserializer.stringArrayReader
            elif t = typeof<int[]> then Some ListDeserializer.intArrayReader
            elif t = typeof<byte[]> then Some ListDeserializer.byteArrayReader
            else
                let elem = t.GetElementType()
                Some <| getTypedArrayReader elem

    /// Try to determine a reader function for list types
    and getListReader (t : Type) = fun () ->
        let matchInterface iface = t.IsAssignableFrom(iface) || t.HasInterface(iface)
        if TypeHelper.isGenericType t then
            let iList = typedefof<IList<_>>
            let hashSet = typedefof<HashSet<_>>
            let queue = typedefof<Queue<_>>
            let stack = typedefof<Stack<_>>
            let linkedList = typedefof<LinkedList<_>>
            let roColl = typedefof<ReadOnlyCollection<_>>
            match t with
            | GenericTypeOf roColl gen ->
                let param = iList.MakeGenericType([| gen |])
                let ctor = t.GetConstructor([| param |])
                if ctor <> null then Some <| getGenericROReader ctor t gen else None
            | GenericTypeOf iList gen ->
                if TypeHelper.hasGenericTypeDefinitions t [| typedefof<List<_>> |]
                then Some <| getTypedListReader gen
                else Some <| getGenericCollectionReader t gen
            | GenericTypeOf hashSet gen -> Some <| getHashSetReader gen
            | GenericTypeOf queue gen -> Some <| getQueueReader gen
            | GenericTypeOf stack gen -> Some <| getStackReader gen
            | GenericTypeOf linkedList gen -> Some <| getLinkedListReader gen
            | _ -> None
        elif TypeHelper.isOrDerived t typeof<NameValueCollection> then
            getNameValueCollectionReader t
        elif matchInterface typeof<IList> then
            let ctor = Reflection.getConstructorMethod t
            let reader = ListDeserializer.collectionReader ctor >> box
            Some reader
        else None

    and getDictTypeArguments (t : Type) =
        let arguments = t.GetGenericArguments()
        arguments.[0], arguments.[1]

    and getTypedDictionaryReader (t : Type) =
        // TODO: this does not look sane at all
        let reader = Type.GetType("SharpXml.DictionaryDeserializer").GetMethod("dictReader")
        let key, value = getDictTypeArguments t
        let mtd = reader.MakeGenericMethod([| key; value |])
        let keyReader = getReaderFunc key
        let valueReader = getReaderFunc value
        fun (xml : XmlElem) -> mtd.Invoke(null, [| keyReader; valueReader; xml |])

    and getDictionaryReader (t : Type) = fun () ->
        let dictInterface = typeof<IDictionary>
        if t.IsAssignableFrom(dictInterface) || t.HasInterface(dictInterface) then
            match TypeHelper.getTypeWithGenericType t typedefof<Dictionary<_,_>> with
            | Some d ->
                Some <| getTypedDictionaryReader t
            | _ -> None
        else None

    /// Class reader function
    and readClass (builder : TypeBuilderInfo) xml =
        let instance = builder.Ctor.Invoke()
        let rec inner inst elems =
            match elems with
            | (GroupElem(name, _)  as h :: t)
            | (ContentElem(name, _) as h :: t) ->
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
            let! dictReader = getDictionaryReader t
            let! listReader = getListReader t
            let! staticReader = getStaticParseMethod t
            let! stringCtor = getStringTypeConstructor t
            let! parseXmlReader = getStaticParseMethod t
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
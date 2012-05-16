namespace SharpXml

/// Reader function delegate
type ReaderFunc = string -> obj

/// Record type containing the deserialization information
/// for a specific property member
type PropertyReaderInfo = {
    Info : System.Reflection.PropertyInfo
    // TODO: I guess I could get rid of the Option in here
    Reader : ReaderFunc option
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

    let getEnumReader (t : Type) = fun () ->
        if t.IsEnum then Some (fun i -> Enum.Parse(t, i)) else None

    let getValueReader (t : Type) = fun () ->
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

/// Deserialization logic
module Deserializer =

    open System
    open System.Collections.Generic
    open System.Reflection

    open SharpXml.Attempt
    open SharpXml.Extensions

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
        | Some ctor -> Some (fun (v : string) -> ctor.Invoke([| v |]))
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
            Some (fun (v : string) -> parse.Invoke(null, [| v |]))
        | _ -> None

    /// Build the PropertyReaderInfo record based on the given PropertyInfo
    let rec buildReaderInfo (p : PropertyInfo) =
        { Info = p; Reader = determineReader p.PropertyType; Setter = Reflection.getObjSetter p }

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

    /// Class reader function
    and readClass (builder : TypeBuilderInfo) (input : string) =
        let len = input.Length
        let instance = builder.Ctor.Invoke()
        let content = TypeParser.parseAST input 0
        let rec inner inst elems =
            match elems with
            | TypeParser.GroupElem(name, subElements) :: t ->
                match builder.Props.TryGetValue name with
                | true, prop ->
                    // TODO
                    inner inst t
                | _ -> inner inst t
            | TypeParser.ContentElem(name, content) :: t ->
                match builder.Props.TryGetValue name with
                | true, prop when prop.Reader.IsSome ->
                    let reader = prop.Reader.Value
                    prop.Setter.Invoke(inst, reader(content))
                    inner inst t
                | _ -> inner inst t
            | TypeParser.SingleElem _ :: t ->
                // TODO: maybe we want to set the default value in here
                inner inst t
            | [] -> ()
        match content with
        | [ TypeParser.GroupElem(name, subElements) ] -> inner instance subElements
        | _ -> inner instance content
        instance

    /// Try to determine a matching class reader function
    and getClassReader (t : Type) = fun () ->
        if t.IsClass && not t.IsAbstract
        then Some (readClass <| getTypeBuilderInfo t)
        else None

    /// Determine the ReaderFunc delegate for the given Type
    and determineReader (objType : Type) =
        let t = objType.NullableUnderlying()
        let reader = attempt {
            let! enumReader = ValueTypeDeserializer.getEnumReader t
            let! valueReader = ValueTypeDeserializer.getValueReader t
            let! staticReader = getStaticParseMethod t
            let! stringCtor = getStringTypeConstructor t
            let! classReader = getClassReader t
            classReader }
        reader

    and getReaderFunc (t : Type) =
        match (!readerCache).TryGetValue t with
        | true, reader -> reader
        | _ ->
            match determineReader t with
            | Some reader -> Atom.updateAtomDict readerCache t reader
            | _ ->
                let err = sprintf "could not determine deserialization logic for type '%s'" t.FullName
                invalidOp err
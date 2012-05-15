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
    Props : System.Collections.Generic.Dictionary<string, PropertyReaderInfo>
    Ctor : Reflection.EmptyConstructor }

/// Deserialization logic
module Deserializer =

    open System
    open System.Collections.Generic
    open System.Reflection

    /// Name of the static parsing method
    let parseMethodName = "ParseXml"

    /// BindingFlags to find the static parse method
    let parseMethodFlags = BindingFlags.Public ||| BindingFlags.Static

    /// TypeBuilder dictionary
    let propertyCache = ref (Dictionary<Type, TypeBuilderInfo>())

    /// Try to find a constructor of the specified type
    /// with a single string parameter
    let findStringConstructor (t : Type) =
        t.GetConstructors()
        |> Array.tryFind (fun ctor ->
            let ps = ctor.GetParameters()
            ps.Length = 1 && ps.[0].ParameterType = typeof<string>)

    /// Try to get a reader based on a string value constructor
    let getStringTypeConstructor (t : Type) =
        match findStringConstructor t with
        | Some ctor -> Some (fun (v : string) -> ctor.Invoke([| v |]))
        | _ -> None

    /// Try to find the static 'ParseXml' method on the specified type
    let findStaticParseMethod (t : Type) =
        t.GetMethod(parseMethodName, parseMethodFlags, null, [| typeof<string> |], null)
        |> Utils.toOption

    /// Try to get a reader based on the type's static 'ParseXml' method
    let getStaticParseMethod (t : Type) =
        // TODO: maybe use Delegate.CreateDelegate()
        match findStaticParseMethod t with
        | Some parse -> Some (fun (v : string) -> parse.Invoke(null, [| v |]))
        | _ -> None

    let rec buildReaderInfo (p : PropertyInfo) =
        { Info = p; Reader = determineReader p.PropertyType; Setter = Reflection.getObjSetter p }

    and buildTypeBuilderInfo (t : Type) =
        let map =
            Reflection.getSerializableProperties t
            |> Seq.map (fun p -> p.Name, buildReaderInfo p)
            |> dict
        { Type = t
          Props = System.Collections.Generic.Dictionary(map, StringComparer.OrdinalIgnoreCase)
          Ctor = Reflection.getEmptyConstructor t }

    and getTypeBuilderInfo (t : Type) =
        match (!propertyCache).TryGetValue t with
        | true, builder -> builder
        | _ ->
            let builder = buildTypeBuilderInfo t
            Atom.updateAtomDict propertyCache t builder

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
            | TypeParser.SingleElem _ :: t->
                // TODO: maybe we want to set the default value in here
                inner inst t
            | [] -> ()
        match content with
        | [ TypeParser.GroupElem(name, subElements) ] -> inner instance subElements
        | _ -> inner instance content
        instance

    and determineReader (t : Type) =
        if t = typeof<string> then Some box
        elif t = typeof<int> then Some (fun s -> Int32.Parse(s) |> box)
        else readClass (getTypeBuilderInfo t) |> Some

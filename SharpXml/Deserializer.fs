namespace SharpXml

/// Reader function delegate
type ReaderFunc = string -> obj

/// Deserialization logic
module Deserializer =

    open System
    open System.Reflection

    /// Name of the static parsing method
    let parseMethodName = "Parse"

    /// BindingFlags to find the static parse method
    let parseMethodFlags = BindingFlags.Public ||| BindingFlags.Static

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

    /// Try to find the static 'Parse' method on the specified type
    let findStaticParseMethod (t : Type) =
        t.GetMethod(parseMethodName, parseMethodFlags, null, [| typeof<string> |], null)
        |> Utils.toOption

    /// Try to get a reader based on the type's static 'Parse' method
    let getStaticParseMethod (t : Type) =
        match findStaticParseMethod t with
        | Some parse -> Some (fun (v : string) -> parse.Invoke(null, [| v |]))
        | _ -> None
        

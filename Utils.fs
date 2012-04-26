namespace SharpXml

module Atom =

    open System.Collections.Generic
    open System.Threading

    let rec swapRef<'T when 'T : not struct> reference newValue =
        let current = !reference
        let result = Interlocked.CompareExchange<'T>(reference, newValue, current)
        if not (obj.ReferenceEquals(result, current)) then
            Thread.SpinWait 20
            swapRef reference newValue

    let updateAtomDict<'TKey,'TValue when 'TKey : equality> (dict : Dictionary<'TKey, 'TValue> ref) key value =
        let newDict = Dictionary<'TKey, 'TValue>(!dict)
        newDict.[key] <- value
        swapRef dict newDict
        value

module Assembly =

    open System
    open System.IO
    open System.Reflection
    open System.Text.RegularExpressions

    let asmRegex = Regex(@"\S+\s*,\s*(\S+)", RegexOptions.Compiled)

    let getAssemblyName (typeName : string) =
        let m = asmRegex.Match(typeName)
        if m.Success && m.Groups.Count > 1 then Some m.Groups.[1].Value
        else None

    let getAssemblyBinaryPath (asm : Assembly) =
        let codeBase = asm.CodeBase
        let path = codeBase.Substring(0, codeBase.LastIndexOf('/') + 1)
        if path.StartsWith("file:///") then
            path.Remove(0, 8)
        else
            path

    let loadAssembly asm =
        let assembly = Assembly.LoadFrom(asm)
        if assembly <> null then Some assembly else None

    let getType (typeName : string) (asm : Assembly)  =
        let t = asm.GetType(typeName)
        if t <> null then Some t else None

    let findTypeFromLoadedAssembly (typeName : string) =
        AppDomain.CurrentDomain.GetAssemblies()
        |> Array.tryPick (getType typeName)

    let findTypeFromAssembly (typeName : string) (asm : string) =
        match findTypeFromLoadedAssembly typeName with
        | Some t -> Some t
        | _ ->
            let bin = getAssemblyBinaryPath <| Assembly.GetExecutingAssembly()
            let tryLoadAssembly ext =
                let file = bin + "." + ext
                if File.Exists file then loadAssembly file else None
            match tryLoadAssembly "dll" with
            | Some a -> getType typeName a
            | _ ->
                match tryLoadAssembly "exe" with
                | Some a -> getType typeName a
                | _ -> None

    let findType (typeName : string) =
        let t = Type.GetType(typeName)
        if t <> null then Some t else
        match getAssemblyName typeName with
        | Some asm -> findTypeFromAssembly typeName asm
        | _ -> findTypeFromLoadedAssembly typeName
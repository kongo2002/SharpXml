namespace SharpXml

/// General purpose utility functions
module internal Utils =

    open System
    open System.Text.RegularExpressions

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.DerivedPatterns
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Reflection

    let genericRegex = Regex("`\d+$", RegexOptions.Compiled)

    /// Wrap a reference (nullable) type into an Option
    let toOption item = if item = null then None else Some item

    /// Turn a tuple typically returned by 'TryGetValue'
    /// functions into an Option type
    let tryToOption = function
        | true, value -> Some value
        | _ -> None

    /// Convenience function for IsNullOrWhiteSpace
    let inline empty str = String.IsNullOrWhiteSpace(str)

    /// Remove the name suffix of a generic type name
    let removeGenericSuffix input =
        genericRegex.Replace(input, String.Empty)

    /// Throw a NotImplementedException
    let notImplemented msg = NotImplementedException(msg) |> raise

    /// Generete a Union type predicate function
    let isUnionCase (c : Expr<_ -> 'T>) =
        match c with
        | Lambdas(_, NewUnionCase(uc, _)) ->
            let tagReader = FSharpValue.PreComputeUnionTagReader(uc.DeclaringType)
            fun (v : 'T) -> (tagReader v) = uc.Tag
        | _ -> failwith "invalid expression"

/// Module containing atomic operations like
/// thread-safe dictionary update
module internal Atom =

    open System.Collections.Generic
    open System.Threading

    /// Atomically swap the specified reference cell
    let rec swapRef<'T when 'T : not struct> reference newValue =
        let current = !reference
        let result = Interlocked.CompareExchange<'T>(reference, newValue, current)
        if not (obj.ReferenceEquals(result, current)) then
            swapRef reference newValue

    /// Atomically update the specified dictionary
    let updateAtomDict<'TKey,'TValue when 'TKey : equality> (dict : Dictionary<'TKey, 'TValue> ref) key value =
        let newDict = Dictionary<'TKey, 'TValue>(!dict)
        newDict.[key] <- value
        swapRef dict newDict
        value

    /// Atomically remove an element from the specified dictionary
    let removeAtomDictElement<'TKey, 'TValue when 'TKey : equality> (dict : Dictionary<'TKey, 'TValue> ref) key =
        let newDict = Dictionary<'TKey, 'TValue>(!dict)
        newDict.Remove(key) |> ignore
        swapRef dict newDict

/// Module containing Assembly related helper functions
module internal Assembly =

    open System
    open System.IO
    open System.Reflection
    open System.Text.RegularExpressions

    let asmRegex = Regex(@"^\S+\s*,\s*([^ \t,]+)", RegexOptions.Compiled)

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
        Assembly.LoadFrom(asm) |> Utils.toOption

    let getType (typeName : string) (asm : Assembly)  =
        asm.GetType(typeName) |> Utils.toOption

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

/// Module containing the Attempt computation builder
module Attempt =

    let bind proc f =
        let value = proc()
        match value with
        | Some _ -> value
        | _ -> f()

    /// Attempt computation builder
    type AttemptBuilder() =
        member this.Return(v) = Some v
        member this.Bind(p, f) = bind p f
        member this.Delay(f) = f()
        member this.Zero() = None
        member this.ReturnFrom(v) = v

    let attempt = AttemptBuilder()

/// Module containing often used generic type definitions
module internal GenericTypes =

    open System.Collections.Generic
    open System.Collections.ObjectModel

    let iList = typedefof<IList<_>>
    let iDict = typedefof<IDictionary<_, _>>
    let iEnum = typedefof<IEnumerable<_>>
    let iColl = typedefof<ICollection<_>>
    let list = typedefof<List<_>>
    let dict = typedefof<Dictionary<_, _>>
    let hashSet = typedefof<HashSet<_>>
    let queue = typedefof<Queue<_>>
    let stack = typedefof<Stack<_>>
    let linkedList = typedefof<LinkedList<_>>
    let roColl = typedefof<ReadOnlyCollection<_>>


/// Module containing various helper functions related to types
module internal TypeHelper =

    open System

    let getGenericType t =
        let rec inner (current : Type) =
            if current = null then None
            elif current.IsGenericType then Some current
            else inner current.BaseType
        inner t

    let isGenericType t =
        let rec inner (current : Type) =
            if current = null then false
            elif current.IsGenericType then true
            else inner current.BaseType
        inner t

    let isOrDerived t baseType =
        let rec inner (current : Type) =
            if current = null then false
            elif current = baseType then true
            else inner current.BaseType
        inner t

    let isOrDerivedIn t baseTypes =
        Seq.exists (isOrDerived t) baseTypes

    let getTypeWithGenericType (t : Type) (genericType : Type) =
        let genInterface =
            t.GetInterfaces()
            |> Array.tryFind(fun x -> x.IsGenericType && x.GetGenericTypeDefinition() = genericType)
        match genInterface with
        | Some _ -> genInterface
        | _ ->
            match getGenericType t with
            | Some genType as gt when genType.GetGenericTypeDefinition() = genericType -> gt
            | _ -> None

    let isTypeWithGenericType (t : Type) (genericType : Type) =
        (getTypeWithGenericType t genericType).IsSome

    let hasGenericTypeDefinitions (t : Type) (genericTypes : Type seq) =
        if not t.IsGenericType then false
        else
            let genTypeDef = t.GetGenericTypeDefinition()
            Seq.exists ((=) genTypeDef) genericTypes

    /// Active pattern wrapper for generic type detection with 1 type argument
    let (|GenericTypeOf|_|) (genericType : Type) (t : Type) =
        match getTypeWithGenericType t genericType with
        | Some genType -> Some <| genType.GetGenericArguments().[0]
        | _ -> None

    /// Active pattern wrapper for generic type detection with 2 type arguments
    let (|GenericTypesOf|_|) (genericType : Type) (t : Type) =
        match getTypeWithGenericType t genericType with
        | Some genType ->
            let args = t.GetGenericArguments()
            if args.Length > 1 then Some(args.[0], args.[1]) else None
        | _ -> None

namespace SharpXml

module Reflection =

    open System
    open System.Collections.Generic
    open System.Reflection
    open System.Reflection.Emit
    open SharpXml.Extensions

    type EmptyConstructor = delegate of unit -> obj
    type SetValueFunc = delegate of obj * obj -> unit
    type ParseFunc = delegate of string -> obj

    let publicFlags =
        BindingFlags.FlattenHierarchy |||
        BindingFlags.Public |||
        BindingFlags.Instance

    let getProps (t : Type) = t.GetProperties(publicFlags)

    let getInterfaceProperties (t : Type) =
        if not t.IsInterface then failwithf "Type '%s' is no interface type" t.FullName
        let map = HashSet<PropertyInfo>(getProps t)
        t.GetInterfaces()
        |> Array.map getProps
        |> Array.concat
        |> Array.iter (map.Add >> ignore)
        Seq.toArray map

    let getPublicProperties (t : Type) =
        if t.IsInterface then getInterfaceProperties t else getProps t

    let getSerializableProperties (t : Type) =
        if t.IsDTO() then
            getPublicProperties t
            |> Array.filter (fun p -> p.IsDataMember())
            |> Seq.ofArray
        else
            getPublicProperties t
            |> Array.filter (fun p ->
                p.GetGetMethod() <> null &&
                hasAttribute p "IgnoreDataMemberAttribute" |> not)
            |> Seq.ofArray

    let getEmptyConstructor (t : Type) =
        let ctor = t.GetConstructor(Type.EmptyTypes)
        if ctor <> null then
            let dm = DynamicMethod("CustomCtor", t, Type.EmptyTypes, t.Module, true)
            let il = dm.GetILGenerator()

            il.Emit(OpCodes.Nop)
            il.Emit(OpCodes.Newobj, ctor)
            il.Emit(OpCodes.Ret)

            dm.CreateDelegate(typeof<EmptyConstructor>) :?> EmptyConstructor
        else
            // this one is for anonymous types that do not have empty constructors
            System.Runtime.Serialization.FormatterServices.GetUninitializedObject(t) :?> EmptyConstructor

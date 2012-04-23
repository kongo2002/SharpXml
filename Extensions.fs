namespace SharpXml

open System
open System.Reflection

module Extensions =

    let caseDiff = (int 'A') - (int 'a')

    type System.String with

        /// Convert the given string to CamelCase form
        member x.ToCamelCase() =
            if x <> null && x.Length > 0 && x.[0] >= 'A' && x.[0] <= 'Z' then
                (char (int x.[0] - caseDiff)).ToString() + x.Substring(1)
            else
                x

    let dataContract = "DataContractAttribute"
    let dataMember = "DataMemberAttribute"

    let hasAttribute (t : MemberInfo) (attrName : string) =
        t.GetCustomAttributes(true)
        |> Array.exists (fun x -> x.GetType().Name = attrName)

    let hasAttributeType (t : MemberInfo) (attrType : Type) =
        t.GetCustomAttributes(attrType, true).Length > 0

    let getAttribute<'a> (t : MemberInfo) =
        let attr = t.GetCustomAttributes(typeof<'a>, true)
        if attr.Length > 0 then Some (attr.[0] :?> 'a) else None

    type System.Reflection.MemberInfo with

        member x.IsDTO() =
            hasAttribute x dataContract

        member x.IsDataMember() =
            hasAttribute x dataMember




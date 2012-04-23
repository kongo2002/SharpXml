namespace SharpXml

open System
open System.Collections.Generic

type ParserFunc = delegate of string -> obj

module TypeParser =

    type ParseState =
        | Start
        | TagStart
        | TagName of int
        | InTag of string

    let mutable cachedParsers = Dictionary<Type, ParserFunc>()

    let parseRawString (input : string) =
        box input

    let isWhitespace c =
        c = ' ' || c = '\t' || c = '\r' || c = '\n'

    let rec skipWhitespace (input : string) index =
        if index >= input.Length || not (isWhitespace input.[index]) then index
        else skipWhitespace input (index + 1)

    let eatTag (input : string) index =
        let start = skipWhitespace input index
        let rec inner i state =
            let len = input.Length
            let next = i + 1
            if next > len then 0, null else
                match state with
                | Start ->
                    if input.[i] = '<' then inner next TagStart else inner next state
                | TagStart ->
                    if isWhitespace input.[i] then
                        inner next state
                    else
                        inner next (TagName i)
                | TagName s ->
                    if isWhitespace input.[i] || input.[i] = '/' then
                        let tag = input.Substring(s, (i-s))
                        inner next (InTag tag)
                    elif input.[i] = '>' then
                        let tag = input.Substring(s, (i-s))
                        i, tag
                    else
                        inner next state
                | InTag tag ->
                    if input.[i] = '>' then i, tag else inner next state
        inner start Start

    let parseArray (t : Type) (input : string) =
        if not t.IsArray then
            let typeName = t.FullName
            failwithf "Type %s is not an array type" typeName

    let getParser targetType =
        match cachedParsers.TryGetValue targetType with
        | true, p -> Some p
        | _ ->
            let nullable = Nullable.GetUnderlyingType(targetType)
            let t = if nullable = null then targetType else nullable
            if t = typeof<string> then
                Some <| ParserFunc parseRawString
            elif t.IsArray then
                Some <| ParserFunc (fun x -> Enum.Parse(t, x, true))
            else
                None

namespace SharpXml

open System
open System.Collections.Generic

type ParserFunc = delegate of string -> obj

module TypeParser =

    type XmlElem =
        | SingleElem of string
        | ContentElem of string * string
        | GroupElem of string * XmlElem list

    type TagType =
        | Open
        | Single
        | Close

    type ParseState =
        | Start
        | TagStart
        | TagName of int * bool
        | InTag of string * TagType

    let whitespaceChars =
        let whitespace = [| ' '; '\t'; '\r'; '\n' |]
        let max =  Array.max whitespace |> int
        Array.init (max+1) (fun c -> Array.exists ((=) (char c)) whitespace)

    let parseRawString (input : string) =
        box input

    let isWhitespace (c : char) =
        let i = int c
        i < whitespaceChars.Length && whitespaceChars.[i]

    let rec skipWhitespace (input : string) index =
        if index >= input.Length || not (isWhitespace input.[index]) then index
        else skipWhitespace input (index + 1)

    /// Eat a XML tag and return its name, the end index and
    /// type being one of Open, Close or Single
    let eatTag (input : string) index =
        let start = skipWhitespace input index
        let len = input.Length
        let rec inner i state =
            let next = i + 1
            if next > len then 0, null, Open else
                match state with
                | Start ->
                    if input.[i] = '<' then inner next TagStart else inner next state
                | TagStart ->
                    if isWhitespace input.[i] then
                        inner next state
                    elif input.[i] = '/' then
                        inner (next+1) (TagName(i+1, true))
                    else
                        inner next (TagName(i, false))
                | TagName(s, close) ->
                    if isWhitespace input.[i] then
                        if not close then
                            let tag = input.Substring(s, (i-s))
                            inner next (InTag (tag, Open))
                        else
                            inner next state
                    elif input.[i] = '/' then
                        let tag = input.Substring(s, (i-s))
                        inner next (InTag (tag, Single))
                    elif input.[i] = '>' then
                        let tag = input.Substring(s, (i-s))
                        let tagType = if close then Close else Open
                        i, tag, tagType
                    else
                        inner next state
                | InTag (tag, tagType) ->
                    if input.[i] = '>' then
                        i, tag, tagType
                    elif input.[i] = '/' then
                        inner next (InTag(tag, Single))
                    else inner next state
        inner start Start

    /// Eat the content of a XML tag and return the
    /// string value as well as the end index
    let eatContent (input : string) index =
        let len = input.Length
        let replace (f : string) (t : string) (i : string) =
            i.Replace(f, t)
        let rec inner i =
            let next = i + 1
            // end of string, this is probably an error
            if next > len then input.Substring(index), len
            elif input.[i] = '<' then
                let length = i - index
                input.Substring(index, length), i
            else inner next
        // TODO: this replacements probably could be done more performant,
        // like while doing the search for the end tag
        let result, endIndex = inner index
        result |> replace "&gt;" ">" |> replace "&lt;" "<", endIndex

    /// Parse the given input string starting from the specified
    /// index into an XML AST
    let parseAST (input : string) index =
        let len = input.Length
        let rec inner i level elements =
            let next = i + 1
            if level = 0 || next >= len then elements, next
            else
                let current = skipWhitespace input i
                match eatTag input current with
                | _, null, _ -> failwith "Unable to read XML tag"
                | x, name, Open when len > x + 1 ->
                    if input.[x+1] = '<' then
                        let elems, endIndex = inner (x+1) 1 []
                        inner endIndex level (GroupElem(name, elems) :: elements)
                    else
                        // start of plain content
                        let content, ind = eatContent input (x+1)
                        let contentEnd, _, _ = eatTag input ind
                        inner (contentEnd+1) level (ContentElem(name, content) :: elements)
                | x, name, Single ->
                    inner (x+1) level (SingleElem name :: elements)
                | x, name, Close ->
                    inner x (level-1) elements
                | _ -> failwith "number of opening and closing XML tags does not match"
        if input.[index] <> '<' then failwith "XML content does not start with '<'"
        inner index 1 []


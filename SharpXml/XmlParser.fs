//  Copyright 2012 Gregor Uhlenheuer
//
//  Licensed under the Apache License, Version 2.0 (the "License");
//  you may not use this file except in compliance with the License.
//  You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.

namespace SharpXml

#nowarn "9"
#nowarn "51"

[<Struct>]
type internal ParserInfo(value : string) =
    [<DefaultValue>]
    val mutable Index : int
    member x.Value = value.ToCharArray()
    member x.Length = value.Length

module internal XmlParser =

    open System
    open System.Collections.Generic
    open Microsoft.FSharp.NativeInterop

    type XmlElem =
        | SingleElem of string
        | ContentElem of string * string
        | GroupElem of string * XmlElem list

    type TagType =
        | Open = 0
        | Single = 1
        | Close = 2

    type ParseState =
        | Start = 0
        | Tag = 1
        | TagName = 2
        | EndTag = 3

    let whitespaceChars =
        let whitespace = [| ' '; '\t'; '\r'; '\n' |]
        let max =  Array.max whitespace |> int
        Array.init (max+1) (fun c -> Array.exists ((=) (char c)) whitespace)

    let parseRawString (input : string) =
        box input

    let inline isWhitespace (c : char) =
        let i = int c
        i < whitespaceChars.Length && whitespaceChars.[i]

    let rec skipWhitespace (input : string) index =
        if index >= input.Length || not (isWhitespace input.[index]) then index
        else skipWhitespace input (index + 1)

    /// Eat a XML tag and return its name, the end index and
    /// type being one of Open, Close or Single
    let eatTag (input : char[]) index =
        let len = input.Length
        let mutable i = index
        let mutable name = Unchecked.defaultof<string>
        let mutable nameStart = 0
        let mutable tag = TagType.Single
        let mutable buffer = &&input.[index]
        let mutable close = false
        let mutable state = ParseState.Start
        let mutable found = false

        while i < len && not found do
            let chr = NativePtr.read buffer
            i <- i + 1
            buffer <- NativePtr.add buffer 1
            match state with
            | ParseState.Start ->
                if chr = '<' then state <- ParseState.Tag
            | ParseState.Tag ->
                if not (isWhitespace chr) then
                    if chr = '/' then
                        close <- true
                        nameStart <- i
                    else
                        nameStart <- i - 1
                    state <- ParseState.TagName
            | ParseState.TagName ->
                if isWhitespace chr then
                    if not close then
                        name <- String(input, nameStart, (i-nameStart-1))
                        tag <- TagType.Open
                        state <- ParseState.EndTag
                elif chr = '/' then
                    name <- String(input, nameStart, (i-nameStart-1))
                    tag <- TagType.Single
                    state <- ParseState.EndTag
                elif chr = '>' then
                    name <- String(input, nameStart, (i-nameStart-1))
                    tag <- if close then TagType.Close else TagType.Open
                    i <- i - 1
                    found <- true
            | _ ->
                if chr = '>' then
                    i <- i - 1
                    found <- true
                elif chr = '/' then
                    tag <- TagType.Single

        i, name, tag

    /// Eat the content of a XML tag and return the
    /// string value as well as the end index
    let eatContent (input : char[]) start =
        let length = input.Length - start
        let mutable len = length
        let mutable found = false
        let mutable encoded = false
        let mutable buffer = &&input.[start]
        while not found do
            if len > 0 then
                let chr = NativePtr.read buffer
                if chr = '<' then
                    found <- true
                else
                    if chr = '&' then encoded <- true
                    len <- len - 1
                    buffer <- NativePtr.add buffer 1
            else
                found <- true
        let result, index = String(input, start, length - len), (length - len + start)
        if encoded then
            result.Replace("&gt;", ">").Replace("&lt;", "<"), index
        else
            result, index

    /// Parse the given input string starting from the specified
    /// index into an XML AST
    let parseAST (input : string) index =
        let inp = input.ToCharArray()
        let len = inp.Length
        let rec inner i level elements =
            let next = i + 1
            if level = 0 || next >= len then elements, next
            else
                match eatTag inp i with
                // open tag
                | x, name, TagType.Open when len > x + 1 ->
                    if inp.[x+1] = '<' then
                        // nested group element
                        let elems, endIndex = inner (x+1) 1 []
                        inner endIndex level (GroupElem(name, elems) :: elements)
                    else
                        // plain content tag
                        let content, ind = eatContent inp (x+1)
                        let contentEnd, _, _ = eatTag inp ind
                        inner (contentEnd+1) level (ContentElem(name, content) :: elements)
                // single tag
                | x, name, TagType.Single ->
                    inner (x+1) level (SingleElem name :: elements)
                // closing tag
                | x, name, TagType.Close ->
                    inner x (level-1) elements
                | _, null, _ -> failwith "Unable to read XML tag"
                | _ -> failwith "number of opening and closing XML tags does not match"
        if inp.[index] <> '<' then failwith "XML content does not start with '<'"
        inner index 1 [] |> fst


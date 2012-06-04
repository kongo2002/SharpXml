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

module internal XmlParser =

    open System
    open System.Collections.Generic

    type XmlElem =
        | SingleElem of string
        | ContentElem of string * string
        | GroupElem of string * XmlElem list

    type TagType =
        | Open = 0
        | Single = 1
        | Close = 2

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
        let nothing = 0, null, TagType.Open

        let rec endTag i name tagType =
            if i >= len then nothing
            else
                let chr = input.[i]
                if chr = '>' then
                    i, name, tagType
                elif chr = '/' then
                    endTag (i+1) name TagType.Single
                else endTag (i+1) name tagType

        let rec getName i start close =
            if i >= len then nothing
            else
                let chr = input.[i]
                if isWhitespace chr then
                    if not close then
                        let tag = String(input, start, (i-start))
                        endTag (i+1) tag TagType.Open
                    else
                        getName (i+1) start close
                elif chr = '/' then
                    let tag = String(input, start, (i-start))
                    endTag (i+1) tag TagType.Single
                elif chr = '>' then
                    let tag = String(input, start, (i-start))
                    let tagType = if close then TagType.Close else TagType.Open
                    i, tag, tagType
                else
                    getName (i+1) start close

        let rec findName i =
            if i >= len then nothing
            else
                let chr = input.[i]
                if isWhitespace chr then
                    findName (i+1)
                elif chr = '/' then
                    getName (i+1) (i+1) true
                else getName (i+1) i false

        let rec findStart i =
            if i >= len then nothing
            elif input.[i] = '<' then findName (i+1)
            else findStart (i+1)

        findStart index

    /// Eat the content of a XML tag and return the
    /// string value as well as the end index
    let eatContent (input : char[]) index =
        let len = input.Length
        let replace (f : string) (t : string) (i : string) =
            i.Replace(f, t)
        let rec inner i esc =
            let next = i + 1
            // end of string, this is probably an error
            if next > len then String(input, index, len), len, esc
            elif input.[i] = '<' then
                let length = i - index
                String(input, index, length), i, esc
            elif input.[i] = '&' then
                inner next true
            else inner next esc
        // TODO: this replacements probably could be done more performant,
        // like while doing the search for the end tag
        let result, endIndex, escaped = inner index false
        if escaped then
            result |> replace "&gt;" ">" |> replace "&lt;" "<", endIndex
        else
            result, endIndex

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


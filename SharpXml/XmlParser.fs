//  Copyright 2012-2013 Gregor Uhlenheuer
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

module internal XmlParser =

    open System
    open System.Collections.Generic
    open Microsoft.FSharp.NativeInterop

    type ParserInfo(value : string) =
        [<DefaultValue>]
        val mutable Index : int
        [<DefaultValue>]
        val mutable Depth : int
        member x.Value = value.ToCharArray()
        member x.Length = value.Length
        member x.IsEnd with get() = x.Index >= x.Length

    type TagType =
        | Open = 0
        | Single = 1
        | Close = 2

    type ParseState =
        | Start = 0
        | Tag = 1
        | TagName = 2
        | InTag = 3
        | AttrName = 4
        | StartString = 5
        | InString = 6

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

    /// Eat a closing XML tag
    let eatClosingTag (input : ParserInfo) =
        let mutable buffer = &&input.Value.[input.Index]
        let mutable state = ParseState.Start
        let mutable found = false

        while input.Index < input.Length && not found do
            let chr = NativePtr.read buffer
            input.Index <- input.Index + 1
            buffer <- NativePtr.add buffer 1
            match state with
            | ParseState.Start ->
                if chr = '<' then state <- ParseState.Tag
            | _ ->
                if chr = '>' then found <- true

    /// Eat a XML tag and return its name and
    /// type being one of Open, Close or Single
    let eatTag (input : ParserInfo) =
        let mutable name = Unchecked.defaultof<string>
        let mutable nameStart = 0
        let mutable tag = TagType.Single
        let mutable buffer = &&input.Value.[input.Index]
        let mutable close = false
        let mutable state = ParseState.Start
        let mutable found = false

        while input.Index < input.Length && not found do
            let chr = NativePtr.read buffer
            input.Index <- input.Index + 1
            buffer <- NativePtr.add buffer 1
            match state with
            | ParseState.Start ->
                if chr = '<' then state <- ParseState.Tag
            | ParseState.Tag ->
                if not (isWhitespace chr) then
                    if chr = '/' then
                        close <- true
                        nameStart <- input.Index
                    else
                        nameStart <- input.Index - 1
                    state <- ParseState.TagName
            | ParseState.TagName ->
                if isWhitespace chr then
                    if not close then
                        name <- String(input.Value, nameStart, (input.Index-nameStart-1))
                        tag <- TagType.Open
                        state <- ParseState.InTag
                elif chr = '/' then
                    name <- String(input.Value, nameStart, (input.Index-nameStart-1))
                    tag <- TagType.Single
                    state <- ParseState.InTag
                elif chr = '>' then
                    name <- String(input.Value, nameStart, (input.Index-nameStart-1))
                    tag <- if close then TagType.Close else TagType.Open
                    found <- true
            | ParseState.InString ->
                if chr = '"' then
                    state <- ParseState.InTag
            | _ ->
                if chr = '>' then
                    found <- true
                elif chr = '"' then
                    state <- ParseState.InString
                elif chr = '/' then
                    tag <- TagType.Single

        name, tag

    /// Eat a XML tag and return its name, type and a
    /// list of attributes
    let eatTagWithAttributes (input : ParserInfo) =
        let mutable name = Unchecked.defaultof<string>
        let mutable nameStart = 0
        let mutable tag = TagType.Single
        let mutable buffer = &&input.Value.[input.Index]
        let mutable close = false
        let mutable state = ParseState.Start
        let mutable found = false
        let mutable attr = Unchecked.defaultof<string>
        let mutable attributes = []

        // TODO: this function is a copy of 'eatTag'
        // TODO: some refactoring required!

        while input.Index < input.Length && not found do
            let chr = NativePtr.read buffer
            input.Index <- input.Index + 1
            buffer <- NativePtr.add buffer 1
            match state with
            | ParseState.Start ->
                if chr = '<' then state <- ParseState.Tag
            | ParseState.Tag ->
                if not (isWhitespace chr) then
                    if chr = '/' then
                        close <- true
                        nameStart <- input.Index
                    else
                        nameStart <- input.Index - 1
                    state <- ParseState.TagName
            | ParseState.TagName ->
                if isWhitespace chr then
                    if not close then
                        name <- String(input.Value, nameStart, (input.Index-nameStart-1))
                        tag <- TagType.Open
                        state <- ParseState.InTag
                elif chr = '/' then
                    name <- String(input.Value, nameStart, (input.Index-nameStart-1))
                    tag <- TagType.Single
                    state <- ParseState.InTag
                elif chr = '>' then
                    name <- String(input.Value, nameStart, (input.Index-nameStart-1))
                    tag <- if close then TagType.Close else TagType.Open
                    found <- true
            | ParseState.AttrName ->
                if chr = '=' || isWhitespace chr then
                    attr <- String(input.Value, nameStart, (input.Index-nameStart-1))
                    state <- ParseState.StartString
            | ParseState.StartString ->
                if chr = '"' then
                    state <- ParseState.InString
                    nameStart <- input.Index
            | ParseState.InString ->
                if chr = '"' then
                    state <- ParseState.InTag
                    attributes <- (attr, (String(input.Value, nameStart, (input.Index-nameStart-1)))) :: attributes
            | _ ->
                if chr = '>' then
                    found <- true
                elif chr = '/' then
                    tag <- TagType.Single
                elif not (isWhitespace chr) then
                    state <- ParseState.AttrName
                    nameStart <- input.Index - 1

        name, tag, attributes

    /// Eat a XML tag and return its type being one of Open, Close or Single
    let eatSomeTag (input : ParserInfo) =
        let mutable tag = TagType.Open
        let mutable buffer = &&input.Value.[input.Index]
        let mutable close = false
        let mutable state = ParseState.Start
        let mutable found = false
        let mutable beforeTag = input.Index

        while not input.IsEnd && not found do
            let chr = NativePtr.read buffer
            input.Index <- input.Index + 1
            buffer <- NativePtr.add buffer 1
            match state with
            | ParseState.Start ->
                if chr = '<' then
                    state <- ParseState.Tag
                    beforeTag <- input.Index - 1
            | ParseState.Tag ->
                if chr = '>' then
                    found <- true
                elif chr = '/' then
                    close <- true
                    tag <- TagType.Close
                    state <- ParseState.TagName
                elif not (isWhitespace chr) then
                    state <- ParseState.TagName
            | ParseState.InString ->
                if chr = '"' then state <- ParseState.TagName
            | _ ->
                if chr = '>' then
                    found <- true
                elif chr = '"' then
                    state <- ParseState.InString
                elif chr = '/' then
                    tag <- TagType.Single
        tag, beforeTag

    /// Eat all content until the last closed tag
    let eatUnknownTilClosing (input : ParserInfo) =
        let rec inner level =
            let tag, before = eatSomeTag input
            match tag with
            | TagType.Close when level = 0 -> before
            | TagType.Close -> inner (level - 1)
            | TagType.Open -> inner (level + 1)
            | _ -> inner level
        inner 0

    /// Parse the XML root node and the optional <?xml ?> tag
    let private eatRootFunc func (input : ParserInfo) =
        let mutable buffer = &&input.Value.[input.Index]
        let mutable state = ParseState.Start
        let mutable found = false

        // eat optional xml doc tag
        while not input.IsEnd && not found do
            let chr = NativePtr.read buffer
            input.Index <- input.Index + 1
            buffer <- NativePtr.add buffer 1

            match state with
            | ParseState.Start ->
                if chr = '<' then state <- ParseState.Tag
            | ParseState.Tag ->
                if chr = '?' then
                    state <- ParseState.TagName
                elif not (isWhitespace chr) then
                    input.Index <- 0
                    found <- true
            | _ ->
                if chr = '>' then found <- true

        func input

    /// Parse the XML root node and the optional <?xml ?> tag
    let eatRoot input =
        eatRootFunc eatTag |> ignore
        []

    /// Parse the XML root node and the optional <?xml ?> tag
    /// along with optional attributes on the root node
    let eatRootWithAttributes input =
        let _, _, attr = eatRootFunc eatTagWithAttributes input
        attr

    /// Eat the content of a XML tag and return the
    /// string value as well as the end index
    let eatContent (input : ParserInfo) =
        let start = input.Index
        let mutable found = false
        let mutable encoded = false
        let mutable buffer = &&input.Value.[input.Index]
        while not found && input.Index < input.Length do
            let chr = NativePtr.read buffer
            if chr = '<' then
                found <- true
            else
                if chr = '&' then encoded <- true
                input.Index <- input.Index + 1
                buffer <- NativePtr.add buffer 1
        let result = String(input.Value, start, input.Index - start)
        if encoded then
            result.Replace("&gt;", ">").Replace("&lt;", "<")
        else
            result

namespace SharpXml

open System
open System.Collections.Generic

type ParserFunc = delegate of string -> obj

module TypeParser =

    type XmlElem =
        | SingleElem of string
        | ContentElem of string * string
        | GroupElem of string * XmlElem list

    type ParseState =
        | Start
        | TagStart
        | TagName of int
        | InTag of string

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

    let eatTag (input : string) index =
        let start = skipWhitespace input index
        let len = input.Length
        let rec inner i state =
            let next = i + 1
            if next > len then 0, null, false else
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
                        let isSingle = input.[i-1] = '/'
                        i, tag, isSingle
                    else
                        inner next state
                | InTag tag ->
                    if input.[i] = '>' then
                        let isSingle = input.[i-1] = '/'
                        i, tag, isSingle
                    else inner next state
        inner start Start

    let eatContent (input : string) index =
        let start = index
        let len = input.Length
        let replace (f : string) (t : string) (i : string) =
            i.Replace(f, t)
        let rec inner i =
            let next = i + 1
            // end of string, this is probably an error
            if next > len then input.Substring(start)
            elif input.[i] = '<' then input.Substring(start, i - start)
            else inner next
        // TODO: this replacements probably could be done more performant,
        // like while doing the search for the end tag
        inner index |> replace "&gt;" ">" |> replace "&lt;" "<"

    let parse (input : string) index =
        let start = skipWhitespace input index
        if input.[start] <> '<' then failwith "XML content does not start with '<'"
        let inner i elements =
            match eatTag input i with
            | _, null, _ -> failwith "Unable to read XML tag"
            | x, name, single -> ()
        inner start []


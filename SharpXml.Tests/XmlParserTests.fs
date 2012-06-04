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

namespace SharpXml.Tests

module XmlParserTests =

    open System.Diagnostics

    open NUnit.Framework

    open SharpXml
    open SharpXml.Tests.TestHelpers
    open SharpXml.XmlParser

    let private writeAst ast =
        let rec inner ast level =
            let debug str = Debug.WriteLine(System.String(' ', level*2) + str)
            match ast with
            | ContentElem(name, content) :: t ->
                sprintf "Content(%s): %s" name content |> debug
                inner t level
            | GroupElem(name, elems) :: t ->
                sprintf "Group(%s):" name |> debug
                inner elems (level+1)
                inner t level
            | SingleElem(name) :: t ->
                sprintf "Single(%s)" name |> debug
                inner t level
            | [] -> ()
        inner ast 0

    let private parse input =
        let result = parseAST input 0
        writeAst result
        result

    [<Test>]
    let eatTag01() =
        let input = " foo <testTag rest/> <hamEggs/>"
        let index, value, t = eatTag (input.ToCharArray()) 0
        value |> should equal "testTag"
        index |> should equal 19
        t |> should equal TagType.Single

    [<Test>]
    let eatTag02() =
        let input = "<testTag />"
        let index, value, t = eatTag (input.ToCharArray()) 0
        value |> should equal "testTag"
        index |> should equal 10
        t |> should equal TagType.Single


    [<Test>]
    let eatTag03() =
        let input = "< fooBar /> < testTag />"
        let index, value, t = eatTag (input.ToCharArray()) 10
        value |> should equal "testTag"
        index |> should equal 23
        t |> should equal TagType.Single

    [<Test>]
    let eatTag04() =
        let input = "xxxx<fooBar>xxxxxxxxxx"
        let index, value, t = eatTag (input.ToCharArray()) 0
        value |> should equal "fooBar"
        index |> should equal 11
        t |> should equal TagType.Open

    [<Test>]
    let eatTag05() =
        let input = "< fooBar / >"
        let index, value, t = eatTag (input.ToCharArray()) 0
        value |> should equal "fooBar"
        index |> should equal 11
        t |> should equal TagType.Single

    [<Test>]
    let eatTag06() =
        let input = "</fooBar>"
        let index, value, t = eatTag (input.ToCharArray()) 0
        value |> should equal "fooBar"
        index |> should equal 8
        t |> should equal TagType.Close

    [<Test>]
    let eatTag07() =
        let input = "<one/><two/><three/><four/>"
        let index, value, t = eatTag (input.ToCharArray()) 12
        value |> should equal "three"
        index |> should equal 19
        t |> should equal TagType.Single

    [<Test>]
    let eatContent01() =
        let input = "<one>this is a small test</test>"
        let result, index = eatContent (input.ToCharArray()) 5
        result |> should equal "this is a small test"
        index |> should equal 25

    [<Test>]
    let eatContent02() =
        let input = "<one>this is &lt;b&gt;a&lt;/b&gt; small test</test>"
        let result, index = eatContent (input.ToCharArray()) 5
        result |> should equal "this is <b>a</b> small test"
        index |> should equal 44

    [<Test>]
    let eatContent03() =
        let input = "<one>foo bar</one><two>ham eggs</two>"
        let result, index = eatContent (input.ToCharArray()) 23
        result |> should equal "ham eggs"
        index |> should equal 31

    [<Test>]
    let parseAST01() =
        let input = "<one>this is a small test</one>"
        parse input |> should equal [ContentElem("one", "this is a small test")]

    [<Test>]
    let parseAST02() =
        let input = "<one><two>this is a small test</two></one>"
        parse input |> should equal [GroupElem("one", [ContentElem("two", "this is a small test")])]

    [<Test>]
    let parseAST03() =
        let input = "<one><two>this is a small test</two><three/></one>"
        parse input
        |> should equal [ GroupElem("one", [ SingleElem "three"; ContentElem("two", "this is a small test") ]) ]

    [<Test>]
    let parseAST04() =
        let input = "<one><two>this is a small test</two><three/><four>foo bar</four></one>"
        parse input
        |> should equal [ GroupElem("one", [ ContentElem("four", "foo bar"); SingleElem "three"; ContentElem("two", "this is a small test") ]) ]

    [<Test>]
    let parseAST05() =
        let input = "<one><two>this is a small test</two><three/><four><five/><six/></four></one>"
        parse input
        |> should equal [ GroupElem("one", [ GroupElem("four", [ SingleElem "six"; SingleElem "five" ]); SingleElem "three"; ContentElem("two", "this is a small test") ]) ]

    [<Test>]
    let parseAST06() =
        let input = "<one><two>this is a small test</two><three/><four><five/><six/><seven>ham eggs</seven></four></one>"
        parse input
        |> should equal [ GroupElem("one", [ GroupElem("four", [ ContentElem("seven", "ham eggs"); SingleElem "six"; SingleElem "five" ]); SingleElem "three"; ContentElem("two", "this is a small test") ]) ]

    [<Test>]
    let parseAST07() =
        let input = "<one/><two/><three/>"
        parse input |> should equal [ SingleElem "three"; SingleElem "two"; SingleElem "one" ]

    [<Test>]
    let parseAST08() =
        let input = "<one>ham eggs</one><two>foo bar</two><three>cheese bacon</three>"
        parse input |> should equal [ ContentElem("three", "cheese bacon"); ContentElem("two", "foo bar"); ContentElem("one", "ham eggs") ]

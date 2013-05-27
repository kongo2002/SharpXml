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

namespace SharpXml.Tests

module XmlParserTests =

    open System
    open System.Diagnostics

    open NUnit.Framework

    open SharpXml
    open SharpXml.Tests.TestHelpers
    open SharpXml.XmlParser

    let private eat input =
        let info = ParserInfo input
        let name, tag = eatTag info
        info.Index, name, tag

    let private eatAt input at =
        let info = ParserInfo input
        info.Index <- at
        let name, tag = eatTag info
        info.Index, name, tag

    let private eatSome input =
        let info = ParserInfo input
        eatSomeTag info |> fst

    let private eatRoot input =
        let info = ParserInfo input
        eatRoot info
        info.Index

    let private eatUnknown input =
        let info = ParserInfo input
        eatUnknownTilClosing info |> ignore
        info

    let private getContent input at =
        let info = ParserInfo input
        info.Index <- at
        let content = eatContent info
        info.Index, content

    [<Test>]
    let eatTag01() =
        let input = " foo <testTag rest/> <hamEggs/>"
        let index, value, t = eat input
        value |> should equal "testTag"
        index |> should equal 20
        t |> should equal TagType.Single

    [<Test>]
    let eatTag02() =
        let input = "<testTag />"
        let index, value, t = eat input
        value |> should equal "testTag"
        index |> should equal 11
        t |> should equal TagType.Single


    [<Test>]
    let eatTag03() =
        let input = "< fooBar /> < testTag />"
        let index, value, t = eatAt input 10
        value |> should equal "testTag"
        index |> should equal 24
        t |> should equal TagType.Single

    [<Test>]
    let eatTag04() =
        let input = "xxxx<fooBar>xxxxxxxxxx"
        let index, value, t = eat input
        value |> should equal "fooBar"
        index |> should equal 12
        t |> should equal TagType.Open

    [<Test>]
    let eatTag05() =
        let input = "< fooBar / >"
        let index, value, t = eat input
        value |> should equal "fooBar"
        index |> should equal 12
        t |> should equal TagType.Single

    [<Test>]
    let eatTag06() =
        let input = "</fooBar>"
        let index, value, t = eat input
        value |> should equal "fooBar"
        index |> should equal input.Length
        t |> should equal TagType.Close

    [<Test>]
    let eatTag07() =
        let input = "<one/><two/><three/><four/>"
        let index, value, t = eatAt input 12
        value |> should equal "three"
        index |> should equal 20
        t |> should equal TagType.Single

    [<Test>]
    let eatContent01() =
        let input = "<one>this is a small test</test>"
        let index, result = getContent input 5
        result |> should equal "this is a small test"
        index |> should equal 25

    [<Test>]
    let eatContent02() =
        let input = "<one>this is &lt;b&gt;a&lt;/b&gt; small test</test>"
        let index, result = getContent input 5
        result |> should equal "this is <b>a</b> small test"
        index |> should equal 44

    [<Test>]
    let eatContent03() =
        let input = "<one>foo bar</one><two>ham eggs</two>"
        let index, result = getContent input 23
        result |> should equal "ham eggs"
        index |> should equal 31

    [<Test>]
    let eatClosingTag01() =
        let input = "<one>ham eggs</one><two>foo bar</two>"
        let index, _, _ = eatAt input 5
        index |> should equal 19

    [<Test>]
    let eatClosingTag02() =
        let input = "<one>ham eggs</one>"
        let index, _, _ = eatAt input 4
        index |> should equal input.Length
        
    [<Test>]
    let eatRoot01() =
        eatRoot "<root><one>ham eggs</one></root>"
        |> should equal 6

    [<Test>]
    let eatRoot02() =
        eatRoot "   < root><one>ham eggs</one></ root>"
        |> should equal 10

    [<Test>]
    let eatRoot03() =
        eatRoot "<?xml version=\"1.0\"?><root><one>ham eggs</one></ root>"
        |> should equal 27

    [<Test>]
    let eatRoot04() =
        eatRoot "  <?xml version=\"1.0\"?> < root><one>ham eggs</one></ root>"
        |> should equal 31

    [<Test>]
    let eatSomeTag01() =
        eatSome "<foo>" |> should equal TagType.Open

    [<Test>]
    let eatSomeTag02() =
        eatSome "</foo>" |> should equal TagType.Close

    [<Test>]
    let eatSomeTag03() =
        eatSome "<foo />" |> should equal TagType.Single

    [<Test>]
    let eatUnknownTag01() =
        let input = "<item>unknown</item></recipient><message>foobar</message><reference>2414059</reference></item></items>"
        let info = eatUnknown input
        info.Index |> should equal 32

    [<Test>]
    let eatUnknownTag02() =
        let input = "unknown</item></recipient><message>foobar</message><reference>2414059</reference></item></items>"
        let info = eatUnknown input
        info.Index |> should equal 14

    [<Test>]
    let eatUnknownTag03() =
        let input = "<item><inner>unknown</inner><inner></inner></item></recipient><message>foobar</message><reference>2414059</reference></item></items>"
        let info = eatUnknown input
        info.Index |> should equal 62
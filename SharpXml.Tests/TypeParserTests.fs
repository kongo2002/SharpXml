namespace SharpXml.Tests

module TypeParserTests =

    open System.Diagnostics

    open NUnit.Framework

    open SharpXml
    open SharpXml.Tests.TestHelpers
    open SharpXml.TypeParser

    [<Test>]
    let eatTag01() =
        let input = " foo <testTag rest/> <hamEggs/>"
        let index, value, single = TypeParser.eatTag input 0
        value |> should equal "testTag"
        index |> should equal 19
        single |> should equal true

    [<Test>]
    let eatTag02() =
        let input = "<testTag />"
        let index, value, single = TypeParser.eatTag input 0
        value |> should equal "testTag"
        index |> should equal 10
        single |> should equal true


    [<Test>]
    let eatTag03() =
        let input = "< fooBar /> < testTag />"
        let index, value, single = TypeParser.eatTag input 10
        value |> should equal "testTag"
        index |> should equal 23
        single |> should equal true

    [<Test>]
    let eatTag04() =
        let input = "xxxx<fooBar>xxxxxxxxxx"
        let index, value, single = TypeParser.eatTag input 0
        value |> should equal "fooBar"
        index |> should equal 11
        single |> should equal false

    [<Test>]
    let eatTag05() =
        let input = "< fooBar / >"
        let index, value, single = TypeParser.eatTag input 0
        value |> should equal "fooBar"
        index |> should equal 11
        single |> should equal true

    [<Test>]
    let eatContent01() =
        let input = "<one>this is a small test</test>"
        let result, _ = TypeParser.eatContent input 5
        result |> should equal "this is a small test"

    [<Test>]
    let eatContent02() =
        let input = "<one>this is &lt;b&gt;a&lt;/b&gt; small test</test>"
        let result, _ = TypeParser.eatContent input 5
        result |> should equal "this is <b>a</b> small test"

    let writeAst ast =
        let rec inner ast level =
            let debug str = System.Diagnostics.Debug.WriteLine(System.String(' ', level*2) + str)
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

    let parse input =
        let result, _ = TypeParser.parseAST input 0
        writeAst result
        result

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

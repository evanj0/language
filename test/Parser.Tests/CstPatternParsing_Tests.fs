module CstPatternParsing.Tests

open NUnit.Framework
open FParsec

let test string = 
    match run CstPatternParsing.pattern string with
    | Success (node, _, _) -> 
        let result = CstPrinting.pattern node
        Assert.AreEqual(string, result)
    | Failure (message, _, _) -> Assert.Fail(message)

(* pattern *)

[<Test>]
let ``assignment``() = test "let variable = (pattern)"
[<Test>]
let ``variant``() = test "(x, xs) : Type"

(* enclosedPattern *)

[<Test>]
let ``ident``() = test "ident"
[<Test>]
let ``discard``() = test "_"
[<Test>]
let ``unit``() = test "()"
[<Test>]
let ``unit with spaces``() = test "( )"
[<Test>]
let ``parens with one item``() = test "(item)"
[<Test>]
let ``parens with three items``() = test "(1,\"string\",item)"
[<Test>]
let ``parens with spaces``() = test "( true , 1.1 , anotherIdent )"
[<Test>]
let ``empty record``() = test "{}"
[<Test>]
let ``empty record with spaces``() = test "{ }"
[<Test>]
let ``record with one item``() = test "{one =item}" (* the space needs to be there *)
[<Test>]
let ``record with three items``() = test "{this =record,contains =three,items =1}"
[<Test>]
let ``record with spaces``() = test "{ spaces = in , between = every , token = 1 }"
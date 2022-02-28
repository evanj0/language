module CstModuleParsing.Tests

open NUnit.Framework
open FParsec

let testd string =
    match run CstModuleParsing.declaration string with
    | Success (node, _, _) -> 
        let result = CstPrinting.declaration node
        Assert.AreEqual(string, result)
    | Failure (message, _, _) -> Assert.Fail(message)

[<Test>]
let ``import``() = testd "import Long::Module(that,has,filtered,imports)"
[<Test>]
let ``import with spaces``() = testd "import A ( withSpaces , inBetween , identifiers )"
[<Test>]
let ``definition``() = testd "def function : Type"
[<Test>]
let ``binding``() = testd "let name = expression"
[<Test>]
let ``newtype``() = testd "type NewType = Type"
[<Test>]
let ``constraint``() = testd "bound EqualityComparable(a,b)={equals:'a->'b->Boolean}"
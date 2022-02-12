module CstTypeParsing_Tests

open NUnit.Framework
open FParsec

(* quantifiedType *)

let testq string =
    match run CstTypeParsing.quantifiedType string with
    | Success (node, _, _) -> 
        let result = CstPrinting.quantifiedType node
        Assert.AreEqual(string, result)
    | Failure (message, _, _) -> Assert.Fail(message)

[<Test>]
let ``forall with one item``() = testq "'type1 forall type1"
[<Test>]
let ``forall with three items``() = testq "'type forall a,b,c"
[<Test>]
let ``forall with spacing``() = testq "'type forall spaces , in , between"
[<Test>]
let ``where with one item``() = testq "a forall b where C(a)"
[<Test>]
let ``where with three items``() = testq "a forall b where C(a,b,c)"
[<Test>]
let ``where with space``() = testq "a forall b where C ( a , b , c )"

(* type *)

let testt string = 
    match run CstTypeParsing.ty string with
    | Success (node, _, _) -> 
        let result = CstPrinting.ty node
        Assert.AreEqual(string, result)
    | Failure (message, _, _) -> Assert.Fail(message)

[<Test>]
let ``function safe with two types``() = testt "a -> b"
[<Test>]
let ``function unsafe with two types``() = testt "a -!> b"
[<Test>]
let ``function safe and unsafe mixed``() = testt "a -> b -!> c -> d -!> e"

[<Test>]
let ``union with two variants``() = testt "Type1|Type2"
[<Test>]
let ``union with three variants``() = testt "Type1|Type2|Type3"
[<Test>]
let ``union with spaces``() = testt "Type1 | Type2 | Type3"

(* enclosedType *)

[<Test>]
let ``string primitive``() = testt "String"
[<Test>]
let ``integer primitive``() = testt "Integer"
[<Test>]
let ``real primitive``() = testt "Real"
[<Test>]
let ``character primitive``() = testt "Character"
[<Test>]
let ``boolean primitive``() = testt "Boolean"

[<Test>]
let ``nominal type simple``() = testt "ThisIsASimpleTypeName"
[<Test>]
let ``nominal type with colons``() = testt "This::Is::A::Resolved::Type::Name"
[<Test>]
let ``nominal type with one argument``() = testt "Type::With<Another::Type>"
[<Test>]
let ``nominal type with three arguments``() = testt "Type<With,Three,Arguments>"
[<Test>]
let ``nominal type with spacing``() = testt "Type::With < Spaces , In::Between , Tokens >"

[<Test>]
let ``type variable``() = testt "'typeVariable"
[<Test>]
let ``type variable with arguments``() = testt "'a<'b,'c>"
[<Test>]
let ``type variable with spacing``() = testt "'improper < Spacing >"

[<Test>]
let ``unit``() = testt "()"
[<Test>]
let ``unit with space``() = testt "( )"

[<Test>]
let ``parens with one item``() = testt "(Item)"
[<Test>]
let ``parens with three items``() = testt "(This,Has,Three::Items)"
[<Test>]
let ``parens with spacing``() = testt "( Spacing , Between , Tokens )"

[<Test>]
let ``empty record``() = testt "{}"
[<Test>]
let ``empty record with space``() = testt "{ }"

[<Test>]
let ``record with one item``() = testt "{one:Item}"
[<Test>]
let ``record with three items``() = testt "{this:Record,constructor:Has,three:Items}"
[<Test>]
let ``record with spaces``() = testt "{ this : Record , has : Spaces , between : Identifiers }"
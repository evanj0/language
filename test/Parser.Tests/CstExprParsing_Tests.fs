module CstExprParsing.Tests

open NUnit.Framework
open FParsec

let test string = 
    match run CstExprParsing.expr string with
    | Success (node, _, _) -> 
        let result = CstPrinting.expr node
        Assert.AreEqual(string, result)
    | Failure (message, _, _) -> Assert.Fail(message)

(* lambda *)

[<Test>]
let ``lambda with one branch``() = test "| x -> x"
[<Test>]
let ``lambda with three arguments``() = test "| x y z -> (x, y, z)"
[<Test>]
let ``lambda with three branches``() = test "| x y -> x | a b -> a | x xs -> (x, xs)"

(* subExpr *)

[<Test>]
let ``let``() = test "let variable = a value"
[<Test>]
let ``unsafe let``() = test "let! variable = unsafe operation"
[<Test>]
let ``unsafe do``() = test "do! an unsafe operation"

[<Test>]
let ``conditional``() = test "if condition then result else other result"
[<Test>]
let ``conditional nested``() = test "if condition then result else if other condition then other result else last result"

[<Test>]
let ``reference``() = test "ref result of an expression"
[<Test>]
let ``dereference``() = test "deref before passing parameter"

[<Test>]
let ``mutate``() = test "mut result of an expression <- result of another expression"

// ORDERED PRECEDENCE

// Explicit Type
[<Test>]
let ``explicit type with 1 item``() = test "expression : Type"
[<Test>]
let ``explicit type with 2 items``() = test "expression : Type1 : Type2"

// Update
[<Test>]
let ``update expression with 1 item``() = test "record with { field = value, field = value2 }"
[<Test>]
let ``update expression with 2 items``() = test "record with { update = 1 } with { update = 2 }"
[<Test>]
let ``update expression after application and chain``() = test "result of.function application with { update = 1 }"

// Application
[<Test>]
let ``application with 2 item``() = test "function application"
[<Test>]
let ``application with 5 items``() = test "this is an application expression"
[<Test>]
let ``application contianing chain``() = test "function applied.to.chained expression"

// Chain
[<Test>]
let ``chain with 1 item``() = test "var.(chained)"
[<Test>]
let ``chain with 5 items``() = test "var.(a).(series).(of).(chained).(expressions)"
[<Test>]
let ``chain that looks like fields``() = test "record.field.access"

// ATOM

// Identifier
[<Test>]
let ``identifier with letters and numbers``() = test "this_is_an_identifier_123"
[<Test>]
let ``identifier with symbols``() = test "identifier_with_symbols?!"
[<Test>]
let ``identifier with 1 level``() = test "Nested::Identifier"
[<Test>]
let ``identifier with 5 levels``() = test "This::Is::A::Deeply::Nested::Identifier"
[<Test>]
let ``identifier equals``() = test "=="
[<Test>]
let ``identifier add``() = test "+"
[<Test>]
let ``identifier sub``() = test "-"
[<Test>]
let ``identifier mul``() = test "*"
[<Test>]
let ``identifier div``() = test "/"

// Parentheses
[<Test>]
let ``tuple with 5 items``() = test "(1,2,3,4,5)"
[<Test>]
let ``tuple with 2 items``() = test "(1,2)"
[<Test>]
let ``parentheses``() = test "(1)"
[<Test>]
let ``unit with no spaces``() = test "()"
[<Test>]
let ``unit with spaces``() = test "( )"
[<Test>]
let ``tuple with spaces``() = test "( 1 , 1.2 , 5 , true , 'a' )"

// Record

[<Test>]
let ``record with 5 items``() = test @"{a =1,b =2,c =3,d =4,e =5}"
[<Test>]
let ``record with 2 items``() = test @"{a =1,b =2}"
[<Test>]
let ``record with 1 item``() = test @"{a =1}"
[<Test>]
let ``empty record``() = test @"{}"
[<Test>]
let ``empty record with spaces``() = test "{ }"
[<Test>]
let ``record with spaces``() = test "{ a = 'a' , b = 1 , field = 1.1 }"

// List
[<Test>]
let ``list with 5 items``() = test @"[1,2.1,'a',4,5]"
[<Test>]
let ``list with 2 items``() = test @"[1,2]"
[<Test>]
let ``list with 1 item``() = test @"[1]"
[<Test>]
let ``empty list``() = test @"[]"
[<Test>]
let ``empty list with spaces``() = test "[ ]"
[<Test>]
let ``list with spaces``() = test "[ 1 , 2 , 3.4 , 'a' , true ]"

// Block
[<Test>]
let ``block with 5 items``() = test @"@{ 1; 2; 3; 4; 5 }"
[<Test>]
let ``block with 2 items``() = test @"@{ 1; 'a' }"
[<Test>]
let ``block with 1 item``() = test @"@{ 1 }"

// Extern
[<Test>]
let ``extern``() = test @"[<""this is an FFI call"" 1>]"

// Literals
[<Test>]
let ``string literal with escaped characters``() = test @"""ab\n\t12+\\\""\u1234q\uffff0"""
[<Test>]
let ``character literal``() = test @"'\u1234'"
[<Test>]
let ``boolean true``() = test "true"
[<Test>]
let ``boolean false``() = test "false"
[<Test>]
let ``integer literal``() = test "1234"
[<Test>]
let ``integer literal with underscores``() = test "1_2_3_4"
[<Test>]
let ``integer literal with negative suffix``() = test "1_2_3_4_n"
[<Test>]
let ``integer literal with uppercase negative suffix``() = test "1234N"
[<Test>]
let ``real literal``() = test "1.2"
[<Test>]
let ``real literal with negative suffix``() = test "123_456.0n"
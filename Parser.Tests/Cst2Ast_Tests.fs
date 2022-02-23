module Cst2Ast_Tests

open NUnit.Framework
open FParsec

let test input expected = 
    match run CstExprParsing.expr input with
    | Success (node, _, _) ->
        let result = 
            node 
            |> Cst2Ast.expr 
            |> Ast.Expression.signature
        printfn "Expected: %s" expected
        printfn "Got: %s" result
        printfn "Raw CST output:\n%s" (node.ToString())
        printfn "Raw AST output:\n%s"((Cst2Ast.expr node).ToString())
        Assert.AreEqual(expected, result)
    | Failure (message, _, _) -> Assert.Fail(message)

[<Test>]
let ``parens with no items``() = test "()" "(tuple)"
[<Test>]
let ``parens with one item``() = test "(a)" "ident"
[<Test>]
let ``parens with three items``() = test "(a, b, c)" "(tuple ident ident ident)"

[<Test>]
let ``lambda with one branch``() = test "| x -> x" "(func (match [ident] (case [ident] ident)))"
[<Test>]
let ``lambda with three branches``() = test "| x -> 1 | x -> 1.0 | x -> 'a'" "(func (match [ident] (case [ident] int) (case [ident] real) (case [ident] char)))"
[<Test>]
let ``lambda with three parameters and one branch``() = test "| x y z -> 0" "(func (func (func (match [(tuple ident ident ident)] (case [(tuple ident ident ident)] int)))))"
module Inference.Tests

open Parsing
open ResultExtensions
open Range
open Ir
open Identifier
open NUnit.Framework

let function1Name = Ident.fromString "function1"
let function1Type = Type.Function (Type.Primitive Type.Int, Type.Primitive Type.Int)

let parseType str = 
    match Parse.``type`` str with
    | Ok t -> Lowering.Lower.lType t
    | Error e -> 
        printfn "%s" e
        failwith "Type signature parsing in test case failed."

let parseExpr str =
    match Parse.expr str with
    | Ok x -> Lowering.Lower.lExpr x
    | Error e -> 
        printfn "%s" e
        failwith "Expression parsing in test case failed."

let tryInfer expr =
    result {
        let pos = { Position.column = 0; line = 0; index = 0 }
        let globals = 
            [ function1Name, function1Type
              Ident.fromString "function2", parseType "Integer -> Integer -> Boolean" ]
        let env = { Env.currentRange = Range.create pos pos; globals = globals; locals = []; }
        let! resultT = 
            Inference.infer expr { State.index = 0 } (fun _t _env -> []) env
            |> IResult.toResult
            |> Result.map (fun (_state, t, _cs) -> t)
        return resultT
    }

let test expr t =
    match tryInfer expr with
    | Ok resultT -> 
        printfn "Expected: %s" (Type.print t)
        printfn "Got: %s" (Type.print resultT)
        Assert.True(t |> Type.equals resultT)
    | Error e -> Assert.Fail(sprintf "Message: %s\nNote: %s\nRange: %s\n" e.message e.note e.range.Display)

let testStr expr t = test (parseExpr expr) (parseType t)

module Literals =
    open UntypedIr
    [<Test>]
    let ``string literal``() = test (Expr.Literal (Literal.Str "string literal")) (Type.Primitive Type.Str)

module OverloadsAndFunctions =
    open UntypedIr
    [<Test>]
    let ``non-overloaded function``() = test (Expr.Ident function1Name) function1Type

    [<Test>]
    let ``non-overloaded function application``() = test (Expr.App (Expr.Ident function1Name, Expr.Literal (Literal.Int 0))) (Type.Primitive Type.Int)

    [<Test>]
    let ``non-overloaded function 2``() = testStr "function1" "Integer -> Integer"

    [<Test>]
    let ``non-overloaded function application 2``() = testStr "function1 0" "Integer"

    [<Test>]
    let ``non-overloaded partial function application``() = testStr "function2 1" "Integer -> Boolean"

    [<Test>]
    let ``non-overloaded multiple function application``() = testStr "function2 1 2" "Boolean"
    
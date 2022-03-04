module Inference_Tests

open Inference
open Ir
open Parsing

open Range
open Identifier
open ResultExtensions

open NUnit.Framework

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
        let pos = { Position.column = 0; Position.line = 0; Position.index = 0 }
        let range = Range.create pos pos
        let globals = 
            [ Ident.fromString "fnOne", parseType "Integer -> Integer"
              Ident.fromString "fnTwo", parseType "Integer -> Integer -> Boolean"
              Ident.fromString "fnThree", parseType "Integer -> Real -> String -> Character -> Boolean"]
        let env = { Env.currentRange = range; globals = globals; locals = [] }
        let state = { State.index = 0 }
        let constrainer = fun _t _env -> []
        let! _state, t, cs = 
            Inference.inferUnsolvedType Inference.defaultSolver expr state constrainer env
            |> IResult.toResult
        let! cs, t = Solving2.solveConstraints cs t
        let! _ = Solving2.verifyConstraints cs
        return t
    }

let test expr t =
    let expr = parseExpr expr
    match tryInfer expr with
    | Ok resultT ->
        let t = parseType t
        printfn "Expected: %s" (Type.print t)
        printfn "Got: %s" (Type.print resultT)
        Assert.True(t |> Type.equals resultT)
    | Error e -> Assert.Fail(sprintf "Message: %s\nNote: %s\nRange: %s\n" e.message e.note e.range.Display)

[<Test>]
let ``function with one argument``() = test "fnOne 1" "Integer"

[<Test>]
let ``function with two arguments``() = test "fnTwo 1 2" "Boolean"

[<Test>]
let ``function with four arguments``() = test "fnThree 1 1.0 \"string\" 'a'" "Boolean"

[<Test>]
let ``partially applied function``() = test "fnTwo 1" "Integer -> Boolean"
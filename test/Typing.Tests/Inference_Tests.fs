module Inference

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
        let pos = { Position.column = 0; Position.line = 0; Position.index = 0; Position.file = "test" }
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

let expect f expr t : unit =
    let expr = parseExpr expr
    let t = parseType t
    f t (tryInfer expr)

let pass t (result: Result<_, Type.Error>) =
    match result with
    | Ok resultT ->
        printfn "Expected: %s" (Type.print t)
        printfn "Got: %s" (Type.print resultT)
        Assert.True(t |> Type.equals resultT)
    | Error e -> Assert.Fail(sprintf "Message: %s\nNote: %s\nRange: %s\n" e.message e.note e.range.Display)

let fail _ (result: Result<_, Type.Error>) =
    match result with
    | Ok resultT ->
        Assert.Fail(sprintf "Expected failure, got: %s" (Type.print resultT))
    | Error e -> 
        Assert.Pass(sprintf "Expected failure.\nMessage: %s\nNote: %s\nRange: %s\n" e.message e.note e.range.Display)

module Functions =
    
    [<Test>]
    let ``function with one argument``() = expect pass "fnOne 1" "Integer"

    [<Test>]
    let ``function with two arguments``() = expect pass "fnTwo 1 2" "Boolean"

    [<Test>]
    let ``function with four arguments``() = expect pass "fnThree 1 1.0 \"string\" 'a'" "Boolean"

    [<Test>]
    let ``partially applied function``() = expect pass "fnTwo 1" "Integer -> Boolean"

module Failure =
    [<Test>]
    let ``function parameter of incorrect type``() = expect fail "fnOne 'a'" "()"

    [<Test>]
    let ``multiple function parameters of incorrect types``() = expect fail "fnThree 1 1.1 'a'" "()"

    [<Test>]
    let ``application of value to value``() = expect fail "\"not a function\" 1" "()"

    [<Test>]
    let ``application of a value to multiple values``() = expect fail "\"not a function\" 1 1.5" "()"
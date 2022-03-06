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

let globals = 
    [ Ident.fromString "fnOne", parseType "Integer -> Integer"
      Ident.fromString "fnTwo", parseType "Integer -> Integer -> Boolean"
      Ident.fromString "fnThree", parseType "Integer -> Real -> String -> Character -> Boolean"
      Ident.fromString "longFn", parseType "Integer -> Integer -> String -> String -> Real -> Real -> Character -> Character -> Boolean -> Boolean"
      Ident.fromString "tupledFunction", parseType "(String, Integer, Real, Character, Boolean) -> String"
      // overloadedFunction1
      Ident.fromString "overloadedFunction1", parseType "String -> Integer   -> (String, Integer)"
      Ident.fromString "overloadedFunction1", parseType "String -> Real      -> (String, Real)"
      Ident.fromString "overloadedFunction1", parseType "String -> Character -> (String, Character)" ]

let tryInfer expr =
    result {
        let pos = { Position.column = 0; Position.line = 0; Position.index = 0; Position.file = "test" }
        let range = Range.create pos pos
        let env = { Env.currentRange = range; globals = globals; locals = [] }
        let state = { State.index = 0 }
        let constrainer = fun _t _env -> []
        let! _state, t, cs = 
            Inference.infer Inference.defaultSolver expr state constrainer env
            |> IResult.toResult
        let! cs, t = Solving.solveConstraints cs t
        let! _ = Solving.verifyConstraints cs
        return t
    }

let printError (e: Type.Error) =
    sprintf "Type inference failed.\nMessage: %s\nNote: %s\nRange: %s\nTrace: %s\n" e.message e.note e.range.Display (e.trace |> List.fold (fun str s -> sprintf "%s\n%s" str s) "")

let expect f expr t : unit =
    printfn "Expression: %s" expr
    let expr = parseExpr expr
    let t = parseType t
    f t (tryInfer expr)

let pass t (result: Result<_, Type.Error>) =
    match result with
    | Ok resultT ->
        printfn "Expected: %s" (Type.print t)
        printfn "Got: %s" (Type.print resultT)
        Assert.True(t |> Type.isMoreGeneralThan resultT)
    | Error e -> Assert.Fail(printError e)

let fail _ (result: Result<_, Type.Error>) =
    match result with
    | Ok resultT ->
        Assert.Fail(sprintf "Expected failure, got: %s" (Type.print resultT))
    | Error e -> Assert.Pass(printError e)

module Functions =
    
    [<Test>]
    let ``function with one argument``() = expect pass "fnOne 1" "Integer"

    [<Test>]
    let ``function with two arguments``() = expect pass "fnTwo 1 2" "Boolean"

    [<Test>]
    let ``function with four arguments``() = expect pass "fnThree 1 1.0 \"string\" 'a'" "Boolean"

    [<Test>]
    let ``partially applied function``() = expect pass "fnTwo 1" "Integer -> Boolean"

    [<Test>]
    let ``tupled function``() = expect pass "tupledFunction (\"string\", 1, 2.5, 'a', false)" "String"

    [<Test>]
    let ``overloaded function application``() = expect pass "overloadedFunction1 \"string\" 1 : (String, Integer)" "(String, Integer)"

    [<Test>]
    let ``overloaded value``() = expect pass "overloadedFunction1 : String -> Integer -> (String, Integer)" "String -> Integer -> (String, Integer)"

module FailureTests =
    [<Test>]
    let ``function parameter of incorrect type``() = expect fail "fnOne 'a'" "()"

    [<Test>]
    let ``multiple function parameters of incorrect types``() = expect fail "fnThree 1 1.1 'a'" "()"

    
    [<Test>]
    let ``many function parameters of incorrect types``() = expect fail "longFn 1 1.5 1 'a' true 2" "()"

    [<Test>]
    let ``application of value to value``() = expect fail "\"not a function\" 1" "()"

    [<Test>]
    let ``application of a value to multiple values``() = expect fail "\"not a function\" 1 1.5" "()"

    [<Test>]
    let ``application of a value to many values``() = expect fail "\"not a function\" 1 1.5 1 'a' true 2" "()"

    [<Test>]
    let ``tuples of different length``() = expect fail "tupledFunction (1, 5, 'a')" "()"

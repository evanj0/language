module New_Inference_Tests

open Ir
open TypedIr
open Parsing
open Inference

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
    | Ok x -> x |> Lowering.Lower.lExpr |> TypedIr.fromUntyped TypedIr.State.init |> fun (state, expr) -> expr
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
      Ident.fromString "overloadedFunction1", parseType "String -> Character -> (String, Character)"
      // math stuff
      Ident.fromString "+", parseType "Integer -> Integer -> Integer"
      Ident.fromString "+", parseType "Real -> Real -> Real"
      Ident.fromString "==", parseType "Integer -> Integer -> Boolean"
      Ident.fromString "==", parseType "Real -> Real -> Real"
      Ident.fromString "and", parseType "Boolean -> Boolean -> Boolean" ]

let tryInfer expr =
    result {
        let pos =
            { Position.column = 0
              Position.line = 0
              Position.index = 0
              Position.file = "test" }

        let range = Range.create pos pos

        let env =
            { Inference.Env.currentRange = range
              Inference.Env.globals = globals
              Inference.Env.locals = [] }

        let constrainer = fun _t _env -> []

        let! cs =
            Typing.infer Typing.defaultSolver expr constrainer env
            |> Typing.TResult.toResult

        let t = expr.t

        let! cs, t = Solving.solveConstraints cs t
        let! _ = Solving.verifyConstraints cs
        return t
    }

let printError (e: Type.Error) =
    sprintf
        "Type inference failed.\nMessage: %s\nNote: %s\nRange: %s\nTrace: %s\n"
        e.message
        e.note
        e.range.Display
        (e.trace
         |> List.fold (fun str s -> sprintf "%s\n%s" str s) "")

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
        Assert.True(t |> Type.equals resultT)
    | Error e -> Assert.Fail(printError e)

let fail _ (result: Result<_, Type.Error>) =
    match result with
    | Ok resultT -> Assert.Fail(sprintf "Expected failure, got: %s" (Type.print resultT))
    | Error e -> Assert.Pass(printError e)

module IfExpression =
    module ShouldFail =
        [<Test>]
        let ``when type of branches does not match``() = expect fail "if true then 1 else 1.5" "()"

        [<Test>]
        let ``when type of guard is not bool``() = expect fail "if 1 then 1 else 1" "()"

    module ShouldPass =
        [<Test>]
        let ``when type of guard is bool and types of branches agree``() = expect pass "if true then 1 else 2" "Integer"

module Ident =
    module ShouldPass =
        [<Test>]
        let ``when value is not overloaded``() = expect pass "fnOne" "Integer -> Integer"

    module ShouldFail =
        [<Test>]
        let ``when value is overloaded and no specifier is provided``() = expect fail "+" "()"

        // TODO this is failing because the substitution/solving algorithm never stops when cs contains one constraint that cannot be substituted.
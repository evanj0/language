module Inference.Tests

open ResultExtensions
open Range
open Ir
open NUnit.Framework

let tryInfer expr =
    result {
        let pos = { Position.column = 0; line = 0; index = 0 }
        let env = { Env.currentRange = Range.create pos pos; globals = []; locals = []; }
        let! resultT = 
            Inference.infer expr { State.index = 0 } (fun t env -> []) env
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

[<TestFixture>]
module Literals =
    open UntypedIr
    [<Test>]
    let ``string literal``() = test (Expr.Literal (Literal.Str "string literal")) (Type.Primitive Type.Str)